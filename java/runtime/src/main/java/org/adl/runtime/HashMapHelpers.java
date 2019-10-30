package org.adl.runtime;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import org.adl.runtime.sys.types.Pair;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import org.adl.runtime.sys.adlast.ScopedName;

import java.util.ArrayList;
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
        HashMap<K,V> result = new HashMap<>();
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
      public JsonBinding<HashMap<K,V>> jsonBinding() {
        return HashMapHelpers.jsonBinding(keyFactory.jsonBinding(), valueFactory.jsonBinding());
      }
    };
  }

  public static <K,V> HashMap<K,V> create(ArrayList<Pair<K,V>> vals) {
    HashMap<K,V> result = new HashMap<>();
    for (Pair<K,V> p : vals) {
      result.put(p.getV1(),p.getV2());
    }
    return result;
  }

  public static <K,V> JsonBinding<HashMap<K,V>> jsonBinding(
      final JsonBinding<K> bindingK,final JsonBinding<V> bindingV) {
    final Factory<HashMap<K,V>> _factory = factory(bindingK.factory(), bindingV.factory());

    return new JsonBinding<HashMap<K,V>>() {
      @Override
      public Factory<HashMap<K,V>> factory() {
        return _factory;
      };

      @Override
      public JsonElement toJson(HashMap<K,V> value) {
        JsonArray result = new JsonArray();
        for (Map.Entry<K,V> entry : value.entrySet()) {
          JsonObject pair = new JsonObject();
          pair.add("v1", bindingK.toJson(entry.getKey()));
          pair.add("v2", bindingV.toJson(entry.getValue()));
          result.add(pair);
        }
        return result;
      }

      @Override
      public HashMap<K,V> fromJson(JsonElement json) {
        if (!json.isJsonArray()) {
          throw new JsonParseException("expected an array");
        }

        JsonArray array = json.getAsJsonArray();
        HashMap<K,V> result = new HashMap<>();
        for(int i = 0; i < array.size(); i++) {
          try {
            JsonObject pair = JsonBindings.objectFromJson(array.get(i));
            K v1 = JsonBindings.fieldFromJson(pair, "v1", bindingK);
            V v2 = JsonBindings.fieldFromJson(pair, "v2", bindingV);
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
