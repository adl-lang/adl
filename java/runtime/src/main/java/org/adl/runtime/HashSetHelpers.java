package org.adl.runtime;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;

import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import org.adl.runtime.sys.adlast.ScopedName;

import java.util.ArrayList;
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

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("sys.types", "Set");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(valueFactory.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<HashSet<V>> jsonBinding() {
        return HashSetHelpers.jsonBinding(valueFactory.jsonBinding());
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

  public static <T> JsonBinding<HashSet<T>> jsonBinding(final JsonBinding<T> bindingT) {
    final Factory<HashSet<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<HashSet<T>>() {
      @Override
      public Factory<HashSet<T>> factory() {
        return _factory;
      };

      @Override
      public JsonElement toJson(HashSet<T> value) {
        JsonArray result = new JsonArray();
        for (T v : value) {
          result.add(bindingT.toJson(v));
        }
        return result;
      }

      @Override
      public HashSet<T> fromJson(JsonElement json) {
        if (!json.isJsonArray()) {
          throw new JsonParseException("expected an array");
        }

        JsonArray array = json.getAsJsonArray();
        HashSet<T> result = new HashSet<>();
        for(int i = 0; i < array.size(); i++) {
          try {
            T v = bindingT.fromJson(array.get(i));
            result.add(v);
          } catch (JsonParseException e) {
            e.pushIndex(i);
            throw e;
          }
        }
        return result;
      }
    };
  }
};
