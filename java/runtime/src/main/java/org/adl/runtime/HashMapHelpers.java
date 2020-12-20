package org.adl.runtime;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import org.adl.runtime.sys.types.MapEntry;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import org.adl.runtime.sys.adlast.ScopedName;

import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;
import java.util.Map;

public class HashMapHelpers
{
  public static <K,V> Factory<Map<K,V>> factory(final Factory<K> keyFactory,final Factory <V> valueFactory) {
    return new Factory<Map<K,V>>() {
      @Override
      public Map<K,V> create(Map<K,V> other) {
        Map<K,V> result = new HashMap<>();
        for (Map.Entry<K,V> e : other.entrySet()) {
          result.put(keyFactory.create(e.getKey()), valueFactory.create(e.getValue()));
        }
        return result;
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("sys.types", "Map");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(keyFactory.typeExpr());
        params.add(valueFactory.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<Map<K,V>> jsonBinding() {
        return HashMapHelpers.jsonBinding(keyFactory.jsonBinding(), valueFactory.jsonBinding());
      }
    };
  }

  public static <K,V> Map<K,V> create(List<MapEntry<K,V>> vals) {
    Map<K,V> result = new HashMap<>();
    for (MapEntry<K,V> p : vals) {
      result.put(p.getKey(),p.getValue());
    }
    return result;
  }

  public static <K,V> JsonBinding<Map<K,V>> jsonBinding(
      final JsonBinding<K> bindingK,final JsonBinding<V> bindingV) {
    final Factory<Map<K,V>> _factory = factory(bindingK.factory(), bindingV.factory());

    return new JsonBinding<Map<K,V>>() {
      @Override
      public Factory<Map<K,V>> factory() {
        return _factory;
      };

      @Override
      public JsonElement toJson(Map<K,V> value) {
        JsonArray result = new JsonArray();
        for (Map.Entry<K,V> me: value.entrySet()) {
          JsonObject entry = new JsonObject();
          entry.add("k", bindingK.toJson(me.getKey()));
          entry.add("v", bindingV.toJson(me.getValue()));
          result.add(entry);
        }
        return result;
      }

      @Override
      public Map<K,V> fromJson(JsonElement json) {
        if (!json.isJsonArray()) {
          throw new JsonParseException("expected an array");
        }

        JsonArray array = json.getAsJsonArray();
        Map<K,V> result = new HashMap<>();
        for(int i = 0; i < array.size(); i++) {
          try {
            JsonObject pair = JsonBindings.objectFromJson(array.get(i));
            K v1 = JsonBindings.fieldFromJson(pair, "k", bindingK);
            V v2 = JsonBindings.fieldFromJson(pair, "v", bindingV);
            result.put(v1,v2);
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
