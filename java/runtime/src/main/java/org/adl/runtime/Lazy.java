package org.adl.runtime;

import java.util.function.Supplier;

public class Lazy<T> implements Supplier<T>
{
  T value;
  Supplier<T> supplier;

  public Lazy(T value) {
    this.value = value;
    this.supplier = null;
  }

  public Lazy(Supplier<T> supplier) {
    this.value = null;
    this.supplier = supplier;
  }

  @Override
  public synchronized T get() {
    if(supplier != null) {
      this.value = supplier.get();
      this.supplier = null;
    }
    return this.value;
  }

};
