package adl.sys.types;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class Set<T> {

  /* Members */

  private java.util.ArrayList<T> value;

  /* Constructors */

  public Set(java.util.ArrayList<T> value) {
    this.value = java.util.Objects.requireNonNull(value);
  }

  /* Accessors and mutators */

  public java.util.ArrayList<T> getValue() {
    return value;
  }

  public void setValue(java.util.ArrayList<T> newValue) {
    value = java.util.Objects.requireNonNull(newValue);
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
      final Factory<java.util.ArrayList<T>> value = Factories.arrayList(factoryT);

      public Set<T> create() {
        return new Set<T>(value.create());
      }

      public Set<T> create(Set<T> other) {
        return new Set<T>(value.create(other.getValue()));
      }
    };
  }
}
