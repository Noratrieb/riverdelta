import { TY_INT, TY_STRING, TY_UNIT } from "./ast";
import { Span } from "./error";
import { InferContext } from "./typeck";

const SPAN: Span = Span.startOfFile({content: ""});

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

it("should not cycle", () => {
  const infcx = new InferContext();

  const a = infcx.newVar();
  const b = infcx.newVar();

  infcx.assign(a, b, SPAN);
  infcx.assign(b, a, SPAN);

  const aType = infcx.resolveIfPossible(a);
  expect(aType.kind).toEqual("var");

  infcx.assign(a, TY_UNIT, SPAN);

  const bType = infcx.resolveIfPossible(b);
  expect(bType.kind).toEqual("tuple");
});
