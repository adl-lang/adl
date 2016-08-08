package org.adl.runtime;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonPrimitive;

import java.io.IOException;
import java.util.ArrayList;

/**
 *  helper implementations for GSON based serialisation.
 */
public class JsonFactories
{
  public static final JsonFactory<Void> VOID = new JsonFactory<Void>() {
    public JsonElement toJson( Void value ) {
      return JsonNull.INSTANCE;
    }
    public Void fromJson( JsonElement json ) {
      json.getAsJsonNull();
      return null;
    }
  };

  public static final JsonFactory<Boolean> BOOLEAN = new JsonFactory<Boolean>() {
    public JsonElement toJson( Boolean value ) {
      return new JsonPrimitive(value);
    }
    public Boolean fromJson( JsonElement json ) {
      return json.getAsBoolean();
    }
  };

  public static <T> JsonFactory<ArrayList<T>> arrayList(final JsonFactory<T> factoryT) {
    return new JsonFactory<ArrayList<T>>() {
      public JsonElement toJson( ArrayList<T> value ) {
        JsonArray result = new JsonArray();
        for (T v : value) {
          result.add(factoryT.toJson(v));
        }
        return result;
      }
      public ArrayList<T> fromJson( JsonElement json ) {
        ArrayList<T> result = new ArrayList<T>();
        for (JsonElement je : json.getAsJsonArray()) {
          result.add(factoryT.fromJson(je));
        }
        return result;
      }
    };
  }
};
