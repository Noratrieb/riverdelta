import { TY_INT, TY_STRING } from "./ast";
import { DUMMY_SPAN as SPAN } from "./error";
import { InferContext } from "./typeck";

it("should infer types across assignments", () => {
  const infcx = new InferContext();

  const a = infcx.newVar();
  const b = infcx.newVar();
  const c = infcx.newVar();

  infcx.assign(a, b, SPAN);
  infcx.assign(b, c, SPAN);

  infcx.assign(a, TY_INT, SPAN);

  const aTy = infcx.resolveIfPossible(c);
  const bTy = infcx.resolveIfPossible(c);
  const cTy = infcx.resolveIfPossible(c);

  expect(aTy.kind).toEqual("int");
  expect(bTy.kind).toEqual("int");
  expect(cTy.kind).toEqual("int");
});

it("should conflict assignments to resolvable type vars", () => {
  const infcx = new InferContext();

  const a = infcx.newVar();
  const b = infcx.newVar();

  infcx.assign(a, b, SPAN);
  infcx.assign(b, TY_INT, SPAN);

  expect(() => infcx.assign(a, TY_STRING, SPAN)).toThrow();
});
