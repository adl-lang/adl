package org.adl.runtime;

import java.util.HashMap;
import java.util.Map;

public class HashMapHelpers
{
  public static <K,V> Factory<HashMap<K,V>> factory(final Factory<K> keyFactory,final Factory <V> valueFactory) {
    return new Factory<HashMap<K,V>>() {
      @Override
      public HashMap<K,V> create() {
        return new HashMap<>();
      }

      @Override
      public HashMap<K,V> create(HashMap<K,V> other) {
        HashMap<K,V> result = new HashMap<K,V>();
        for (Map.Entry<K,V> e : other.entrySet()) {
          result.put(keyFactory.create(e.getKey()), valueFactory.create(e.getValue()));
        }
        return result;
      }
    };
  }
};
