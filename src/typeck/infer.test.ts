import { Emitter, ErrorHandler, Span } from "../error";
import { TYS } from "../types";
import { InferContext } from "./infer";

const SPAN: Span = Span.startOfFile({ content: "" });

const dummyEmitter: Emitter = () => {};

it("should infer types across assignments", () => {
  const infcx = new InferContext(new ErrorHandler(dummyEmitter));

  const a = infcx.newVar();
  const b = infcx.newVar();
  const c = infcx.newVar();

  infcx.assign(a, b, SPAN);
  infcx.assign(b, c, SPAN);

  infcx.assign(a, TYS.INT, SPAN);

  const aTy = infcx.resolveIfPossible(c);
  const bTy = infcx.resolveIfPossible(c);
  const cTy = infcx.resolveIfPossible(c);

  expect(aTy.kind).toEqual("int");
  expect(bTy.kind).toEqual("int");
  expect(cTy.kind).toEqual("int");
});

it("should conflict assignments to resolvable type vars", () => {
  let errorLines = 0;
  const emitter = () => (errorLines += 1);
  const infcx = new InferContext(new ErrorHandler(emitter));

  const a = infcx.newVar();
  const b = infcx.newVar();

  infcx.assign(a, b, SPAN);
  infcx.assign(b, TYS.INT, SPAN);

  expect(errorLines).toEqual(0);

  infcx.assign(a, TYS.STRING, SPAN);

  expect(errorLines).toBeGreaterThan(0);
});

it("should not cycle", () => {
  const infcx = new InferContext(new ErrorHandler(dummyEmitter));

  const a = infcx.newVar();
  const b = infcx.newVar();

  infcx.assign(a, b, SPAN);
  infcx.assign(b, a, SPAN);

  const aType = infcx.resolveIfPossible(a);
  expect(aType.kind).toEqual("var");

  infcx.assign(a, TYS.UNIT, SPAN);

  const bType = infcx.resolveIfPossible(b);
  expect(bType.kind).toEqual("tuple");
});
