// L32-eval.ts
import { map } from "ramda";
import { AppExp, isCExp, isLetExp } from "./L32-ast";
import { BoolExp, CExp, Exp, IfExp, LitExp, NumExp,
         PrimOp, ProcExp, Program, StrExp, VarDecl, DictExp} from "./L32-ast";  // <- added DictExp
import { isAppExp, isBoolExp, isDefineExp, isIfExp, isLitExp, isNumExp,
             isPrimOp, isProcExp, isStrExp, isVarRef, isDictExp} from "./L32-ast";      // <- added isDictExp
import { makeBoolExp, makeLitExp, makeNumExp, makeProcExp, makeStrExp } from "./L32-ast";
import { parseL32Exp } from "./L32-ast";
import { applyEnv, makeEmptyEnv, makeEnv, Env } from "./L32-env";
import { isClosure, makeClosure, Closure, Value,  makeDictValue, isDictValue, SExpValue, DictValue, isSymbolSExp, isCompoundSExp, CompoundSExp, SymbolSExp } from "./L32-value";   // <- added DictValue, isDictValue, makeDictValue, SExpValue
import { first, rest, isEmpty, List, isNonEmptyList } from '../shared/list';
import { isBoolean, isNumber, isString } from "../shared/type-predicates";
import { Result, makeOk, makeFailure, bind, mapResult, mapv } from "../shared/result";
import { renameExps, substitute } from "./substitute";
import { applyPrimitive } from "./evalPrimitive";
import { parse as p } from "../shared/parser";
import { Sexp } from "s-expression";
import { format } from "../shared/format";



// ========================================================
// Eval functions

const L32applicativeEval = (exp: CExp, env: Env): Result<Value> =>
    isNumExp(exp) ? makeOk(exp.val) : 
    isBoolExp(exp) ? makeOk(exp.val) :
    isStrExp(exp) ? makeOk(exp.val) :
    isPrimOp(exp) ? makeOk(exp) :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isLitExp(exp) ? makeOk(exp.val) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? evalProc(exp, env) :
    isAppExp(exp) ?
        isDictPredicate(exp) ? evalDictPredicate(exp, env) :
            bind(L32applicativeEval(exp.rator, env), (rator: Value) =>
                bind(mapResult(param => L32applicativeEval(param, env), exp.rands), (rands: Value[]) =>
                L32applyProcedure(rator, rands, env))) :
  
    isLetExp(exp) ? makeFailure('"let" not supported (yet)') :
    isDictExp(exp) ?  evalDictExp(exp, env) : // <- added isDictExp
    exp;

const isDictPredicate = (exp: AppExp): boolean =>
    isVarRef(exp.rator) && exp.rator.var === "dict?";

export const unList = (v: Value): Value[] =>
    isCompoundSExp(v)
        ? [v.val1, ...unList(v.val2)]
        : [];

export const hasUniqueDictKeys = (v: Value): boolean => {
    if (!isCompoundSExp(v)) return false;

    const entries = unList(v);

    const keys = entries
        .filter(p => isCompoundSExp(p) && isSymbolSExp(p.val1))
        .map(p => ((p as CompoundSExp).val1 as SymbolSExp).val);

    const uniqueKeys = new Set(keys);
    return keys.length === uniqueKeys.size;
};


const evalDictPredicate = (exp: AppExp, env: Env): Result<Value> => {
    if (exp.rands.length !== 1) {
      return makeFailure("dict? expects exactly one argument");
    }
    return bind(L32applicativeEval(exp.rands[0], env), (argVal: Value) =>
      makeOk(hasUniqueDictKeys(argVal))
    );
  };
  

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: Env): Result<Value> =>
    bind(L32applicativeEval(exp.test, env), (test: Value) => 
        isTrueValue(test) ? L32applicativeEval(exp.then, env) : 
    L32applicativeEval(exp.alt, env));

const evalProc = (exp: ProcExp, env: Env): Result<Closure> =>
    makeOk(makeClosure(exp.args, exp.body));

const L32applyProcedure = (proc: Value, args: Value[], env: Env): Result<Value> =>
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    isClosure(proc) ? applyClosure(proc, args, env) :
    isDictValue(proc) ? applyDictLookup(proc, args) :
    makeFailure(`Bad procedure ${format(proc)}`);

// Applications are computed by substituting computed
// values into the body of the closure.
// To make the types fit - computed values of params must be
// turned back in Literal Expressions that eval to the computed value.
const valueToLitExp = (v: Value): NumExp | BoolExp | StrExp | LitExp | PrimOp | ProcExp =>
    isNumber(v) ? makeNumExp(v) :
    isBoolean(v) ? makeBoolExp(v) :
    isString(v) ? makeStrExp(v) :
    isPrimOp(v) ? v :
    isClosure(v) ? makeProcExp(v.params, v.body) :
    makeLitExp(v);

const applyClosure = (proc: Closure, args: Value[], env: Env): Result<Value> => {
    const vars = map((v: VarDecl) => v.var, proc.params);
    const body = renameExps(proc.body);
    const litArgs = map(valueToLitExp, args);
    return evalSequence(substitute(body, vars, litArgs), env);
}

const applyDictLookup = (dict: DictValue, args: Value[]): Result<Value> => {
    if (args.length !== 1)
        return makeFailure("Dict lookup expects exactly one argument");

    const keyVal = args[0];

    if (isSymbolSExp(keyVal)) {
        const key = keyVal.val;
        return dict.dict.has(key)
            ? makeOk(dict.dict.get(key) as Value)
            : makeFailure(`Key '${key}' not found in dict`);
    } else {
        return makeFailure(`Dict key must be a quoted symbol`);
    }
};

const evalDictExp = (exp: DictExp, env: Env): Result<Value> =>
    mapv( mapResult(([key, valExp]) =>
          bind(L32applicativeEval(valExp, env), (val: Value) =>
            makeOk<[string, Value]>([key, val])),exp.entries),(evaluatedPairs: [string, Value][]) => {
                const dict = new Map<string, Value>();
                evaluatedPairs.forEach(([key, val]) => 
                    dict.set(key, val));
        return makeDictValue(dict);
      }
    );
  

// Evaluate a sequence of expressions (in a program)
export const evalSequence = (seq: List<Exp>, env: Env): Result<Value> =>
    isNonEmptyList<Exp>(seq) ? 
        isDefineExp(first(seq)) ? evalDefineExps(first(seq), rest(seq), env) :
        evalCExps(first(seq), rest(seq), env) :
    makeFailure("Empty sequence");

const evalCExps = (first: Exp, rest: Exp[], env: Env): Result<Value> =>
    isCExp(first) && isEmpty(rest) ? L32applicativeEval(first, env) :
    isCExp(first) ? bind(L32applicativeEval(first, env), _ => 
                            evalSequence(rest, env)) :
    makeFailure("Never");

// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: Exp, exps: Exp[], env: Env): Result<Value> =>
    isDefineExp(def) ? bind(L32applicativeEval(def.val, env), (rhs: Value) => 
                                evalSequence(exps, makeEnv(def.var.var, rhs, env))) :
    makeFailure(`Unexpected in evalDefine: ${format(def)}`);

// Main program
export const evalL32program = (program: Program): Result<Value> =>
    evalSequence(program.exps, makeEmptyEnv());

export const evalParse = (s: string): Result<Value> =>
    bind(p(s), (sexp: Sexp) => 
        bind(parseL32Exp(sexp), (exp: Exp) =>
            evalSequence([exp], makeEmptyEnv())));
