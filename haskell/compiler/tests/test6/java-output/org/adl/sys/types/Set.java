package org.adl.sys.types;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.ArrayList;
import java.util.Objects;

public class Set<T> {

  /* Members */

  private ArrayList<T> value;

  /* Constructors */

  public Set(ArrayList<T> value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Accessors and mutators */

  public ArrayList<T> getValue() {
    return value;
  }

  public void setValue(ArrayList<T> newValue) {
    value = Objects.requireNonNull(newValue);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Set)) {
      return false;
    }
    Set other = (Set) other0;
    return
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + value.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<Set<T>> factory(Factory<T> factoryT) {
    return new Factory<Set<T>>() {
      final Factory<ArrayList<T>> value = Factories.arrayList(factoryT);

      public Set<T> create() {
        return new Set<T>(value.create());
      }

      public Set<T> create(Set<T> other) {
        return new Set<T>(value.create(other.getValue()));
      }
    };
  }
}
