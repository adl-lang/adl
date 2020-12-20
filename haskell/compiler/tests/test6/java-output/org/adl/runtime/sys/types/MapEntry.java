/* @generated from adl module sys.types */

package org.adl.runtime.sys.types;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class MapEntry<K, V> {

  /* Members */

  private K key;
  private V value;

  /* Constructors */

  public MapEntry(K key, V value) {
    this.key = Objects.requireNonNull(key);
    this.value = Objects.requireNonNull(value);
  }

  /* Accessors and mutators */

  public K getKey() {
    return key;
  }

  public void setKey(K key) {
    this.key = Objects.requireNonNull(key);
  }

  public V getValue() {
    return value;
  }

  public void setValue(V value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof MapEntry)) {
      return false;
    }
    MapEntry<?, ?> other = (MapEntry<?, ?>) other0;
    return
      key.equals(other.key) &&
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + key.hashCode();
    _result = _result * 37 + value.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder<K, V> {
    private K key;
    private V value;

    public Builder() {
      this.key = null;
      this.value = null;
    }

    public Builder<K, V> setKey(K key) {
      this.key = Objects.requireNonNull(key);
      return this;
    }

    public Builder<K, V> setValue(V value) {
      this.value = Objects.requireNonNull(value);
      return this;
    }

    public MapEntry<K, V> create() {
      Builders.checkFieldInitialized("MapEntry", "key", key);
      Builders.checkFieldInitialized("MapEntry", "value", value);
      return new MapEntry<K, V>(key, value);
    }
  }

  /* Factory for construction of generic values */

  public static <K, V> Factory<MapEntry<K, V>> factory(Factory<K> factoryK, Factory<V> factoryV) {
    return new Factory<MapEntry<K, V>>() {
      final Lazy<Factory<K>> key = new Lazy<>(() -> factoryK);
      final Lazy<Factory<V>> value = new Lazy<>(() -> factoryV);

      @Override
      public MapEntry<K, V> create(MapEntry<K, V> other) {
        return new MapEntry<K, V>(
          key.get().create(other.getKey()),
          value.get().create(other.getValue())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("sys.types", "MapEntry");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryK.typeExpr());
        params.add(factoryV.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<MapEntry<K, V>> jsonBinding() {
        return MapEntry.jsonBinding(factoryK.jsonBinding(), factoryV.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<K, V> JsonBinding<MapEntry<K, V>> jsonBinding(JsonBinding<K> bindingK, JsonBinding<V> bindingV) {
    final Lazy<JsonBinding<K>> key = new Lazy<>(() -> bindingK);
    final Lazy<JsonBinding<V>> value = new Lazy<>(() -> bindingV);
    final Factory<K> factoryK = bindingK.factory();
    final Factory<V> factoryV = bindingV.factory();
    final Factory<MapEntry<K, V>> _factory = factory(bindingK.factory(), bindingV.factory());

    return new JsonBinding<MapEntry<K, V>>() {
      @Override
      public Factory<MapEntry<K, V>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(MapEntry<K, V> _value) {
        JsonObject _result = new JsonObject();
        _result.add("k", key.get().toJson(_value.key));
        _result.add("v", value.get().toJson(_value.value));
        return _result;
      }

      @Override
      public MapEntry<K, V> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new MapEntry<K, V>(
          JsonBindings.fieldFromJson(_obj, "k", key.get()),
          JsonBindings.fieldFromJson(_obj, "v", value.get())
        );
      }
    };
  }
}
