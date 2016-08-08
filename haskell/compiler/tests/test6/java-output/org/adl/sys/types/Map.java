package org.adl.sys.types;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.ArrayList;
import java.util.Objects;

public class Map<K, V> {

  /* Members */

  private ArrayList<Pair<K, V>> value;

  /* Constructors */

  public Map(ArrayList<Pair<K, V>> value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Accessors and mutators */

  public ArrayList<Pair<K, V>> getValue() {
    return value;
  }

  public void setValue(ArrayList<Pair<K, V>> newValue) {
    value = Objects.requireNonNull(newValue);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Map)) {
      return false;
    }
    Map other = (Map) other0;
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

  public static <K, V> Factory<Map<K, V>> factory(Factory<K> factoryK, Factory<V> factoryV) {
    return new Factory<Map<K, V>>() {
      final Factory<ArrayList<Pair<K, V>>> value = Factories.arrayList(Pair.factory(factoryK, factoryV));

      public Map<K, V> create() {
        return new Map<K, V>(value.create());
      }

      public Map<K, V> create(Map<K, V> other) {
        return new Map<K, V>(value.create(other.getValue()));
      }
    };
  }
}
