export function encodeUtf8(s: string): Uint8Array {
  return new TextEncoder().encode(s);
}

export class Ids {
  nextId: number = 0;

  public next(): number {
    return this.nextId++;
  }
}

export function unwrap<T>(value: T | undefined): T {
  if (value === undefined) {
    throw new Error("tried to unwrap undefined value");
  }
  return value;
}
