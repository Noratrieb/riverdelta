export function encodeUtf8(s: string): Uint8Array {
  return new TextEncoder().encode(s);
}

export class Ids {
  private nextId = 0;

  public next(): number {
    return this.nextId++;
  }
}

export function unwrap<T>(value: T | undefined, msg?: string): T {
  if (value === undefined) {
    throw new Error(msg ?? "tried to unwrap undefined value");
  }
  return value;
}

/**
 * A `Map` that can have arbitrarily complex keys.
 * It uses JSON+string equality instead of refernece equality.
 */
export class ComplexMap<K, V> {
  private inner = new Map<string | number, V>();

  public get(key: K): V | undefined {
    return this.inner.get(mangleKey(key));
  }

  public set(key: K, value: V): void {
    this.inner.set(mangleKey(key), value);
  }
}

export class ComplexSet<K> {
  private inner = new Set();

  public has(key: K): boolean {
    return this.inner.has(mangleKey(key));
  }

  public add(key: K): void {
    this.inner.add(mangleKey(key));
  }
}

function mangleKey<K>(key: K): string | number {
  if (typeof key === "string" || typeof key === "number") {
    return key;
  }
  return JSON.stringify(key);
}
