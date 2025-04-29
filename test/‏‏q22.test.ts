import fs from "fs";
import { expect } from 'chai';
import {  evalL32program } from '../src/L32/L32-eval';
import { Value } from "../src/L32/L32-value";
import { Result, bind, isFailure, makeFailure, makeOk } from "../src/shared/result";
import { parseL32, parseL32Exp } from "../src/L32/L32-ast";
import { makeEmptySExp } from "../src/L3/L3-value";

const evalP = (x: string): Result<Value> =>
    bind(parseL32(x), evalL32program);

describe('Q22 Tests', () => {

    it("Q22 basic tests 1", () => {
        expect(evalP(`(L32 ((dict (a 1) (b 2)) 'a))`)).to.deep.equal(makeOk(1));
    });
    
    it("Q22 tests 2", () => {
        expect(evalP(`(L32
                      (define x "a")
                      (define y "b")
                      ((dict (a x) (b y)) 'b))`)).to.deep.equal(makeOk("b"))
    });

    it("Q22 test 3", () => {
        expect(evalP(`(L32 
            (define x 1)
            (
              (if (< x 0)
                (dict (a 1) (b 2))
                (dict (a 2) (b 1)))
            'a))`)).to.deep.equal(makeOk(2));
    });
    it("Q22 test 4 - dict? detects duplicate keys", () => {
        expect(evalP(`(L32 (dict? '((a . 1) (a . 2))))`)).to.deep.equal(makeOk(false));
    });

    it("Q22 – simple dict lookup returns correct value", () => {
        expect(evalP(`(L32 ((dict (a 1) (b 2)) 'a))`)).to.deep.equal(makeOk(1));
    });

    it("Q22 – dict with variables as values", () => {
        expect(evalP(`(L32
            (define x "hello")
            ((dict (a x) (b "world")) 'a))`)).to.deep.equal(makeOk("hello"));
    });

    it("Q22 – dict lookup fails on missing key", () => {
        expect(evalP(`(L32 ((dict (a 1) (b 2)) 'z))`)).to.satisfy(isFailure);
    });

    it("Q22 – dict? returns true for valid dict", () => {
        expect(evalP(`(L32 (dict? '((a . 1) (b . 2))))`)).to.deep.equal(makeOk(true));
    });

    it("Q22 – dict? returns false for duplicate keys", () => {
        expect(evalP(`(L32 (dict? '((a . 1) (a . 2))))`)).to.deep.equal(makeOk(false));
    });

    it("Q22 – dict fails on non-symbol key", () => {
        expect(evalP(`(L32 ((dict ("a" 1)) 'a))`)).to.satisfy(isFailure);
    });

    it("Q22 – parse fails on duplicate keys", () => {
        expect(evalP(`(L32 ((dict (a 1) (a 2)) 'a))`)).to.satisfy(isFailure);
    });
    
        
    
});