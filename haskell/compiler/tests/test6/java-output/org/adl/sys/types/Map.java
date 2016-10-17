package org.adl.sys.types;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
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

  public void setValue(ArrayList<Pair<K, V>> value) {
    this.value = Objects.requireNonNull(value);
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
      final Lazy<Factory<ArrayList<Pair<K, V>>>> value = new Lazy<>(() -> Factories.arrayList(Pair.factory(factoryK, factoryV)));

      public Map<K, V> create() {
        return new Map<K, V>(
          value.get().create()
          );
      }

      public Map<K, V> create(Map<K, V> other) {
        return new Map<K, V>(
          value.get().create(other.getValue())
          );
      }
    };
  }

  /* Json serialization */

  public static<K, V> JsonBinding<Map<K, V>> jsonBinding(JsonBinding<K> bindingK, JsonBinding<V> bindingV) {
    final JsonBinding<ArrayList<Pair<K, V>>> _binding = JsonBindings.arrayList(Pair.jsonBinding(bindingK, bindingV));
    final Factory<Map<K, V>> _factory = factory(bindingK.factory(), bindingV.factory());

    return new JsonBinding<Map<K, V>>() {
      public Factory<Map<K, V>> factory() {
        return _factory;
      }

      public JsonElement toJson(Map<K, V> _value) {
        return _binding.toJson(_value.value);
      }

      public Map<K, V> fromJson(JsonElement _json) {
        return new Map<K, V>(_binding.fromJson(_json));
      }
    };
  }
}
