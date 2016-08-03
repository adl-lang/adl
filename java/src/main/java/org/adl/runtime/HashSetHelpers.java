package org.adl.runtime;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class HashSetHelpers
{
  public static <V> Factory<HashSet<V>> factory(final Factory <V> valueFactory) {
    return new Factory<HashSet<V>>() {
      @Override
      public HashSet<V> create() {
        return new HashSet<>();
      }

      @Override
      public HashSet<V> create(HashSet<V> other) {
        HashSet<V> result = new HashSet<V>();
        for (V v : other) {
          result.add(valueFactory.create(v));
        }
        return result;
      }
    };
  }

  public static <V> HashSet<V> create(List<V> vals) {
    HashSet<V> result = new HashSet<V>();
    for (V v : vals) {
      result.add(v);
    }
    return result;
  }
};
