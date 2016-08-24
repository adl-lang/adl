package org.adl.runtime;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

/**
 *  helper implementations for GSON based serialisation.
 */
public class JsonBindings
{
  public static final JsonBinding<Void> VOID = new JsonBinding<Void>() {
    public Factory<Void> factory() {
      return Factories.VOID;
    }
    public JsonElement toJson(Void value) {
      return JsonNull.INSTANCE;
    }
    public Void fromJson(JsonElement json) {
      json.getAsJsonNull();
      return null;
    }
  };

  public static final JsonBinding<Boolean> BOOLEAN = new JsonBinding<Boolean>() {
    public Factory<Boolean> factory() {
      return Factories.BOOLEAN;
    }
    public JsonElement toJson(Boolean value) {
      return new JsonPrimitive(value);
    }
    public Boolean fromJson(JsonElement json) {
      return json.getAsBoolean();
    }
  };

  public static final JsonBinding<Byte> BYTE = new JsonBinding<Byte>() {
    public Factory<Byte> factory() {
      return Factories.BYTE;
    }
    public JsonElement toJson(Byte value) {
      return new JsonPrimitive(value);
    }
    public Byte fromJson(JsonElement json) {
      return json.getAsNumber().byteValue();
    }
  };

  public static final JsonBinding<Short> SHORT = new JsonBinding<Short>() {
    public Factory<Short> factory() {
      return Factories.SHORT;
    }
    public JsonElement toJson(Short value) {
      return new JsonPrimitive(value);
    }
    public Short fromJson(JsonElement json) {
      return json.getAsNumber().shortValue();
    }
  };

  public static final JsonBinding<Integer> INTEGER = new JsonBinding<Integer>() {
    public Factory<Integer> factory() {
      return Factories.INTEGER;
    }
    public JsonElement toJson(Integer value) {
      return new JsonPrimitive(value);
    }
    public Integer fromJson(JsonElement json) {
      return json.getAsNumber().intValue();
    }
  };

  public static final JsonBinding<Long> LONG = new JsonBinding<Long>() {
    public Factory<Long> factory() {
      return Factories.LONG;
    }
    public JsonElement toJson(Long value) {
      return new JsonPrimitive(value);
    }
    public Long fromJson(JsonElement json) {
      return json.getAsNumber().longValue();
    }
  };

  public static final JsonBinding<Float> FLOAT = new JsonBinding<Float>() {
    public Factory<Float> factory() {
      return Factories.FLOAT;
    }
    public JsonElement toJson(Float value) {
      return new JsonPrimitive(value);
    }
    public Float fromJson(JsonElement json) {
      return json.getAsNumber().floatValue();
    }
  };

  public static final JsonBinding<Double> DOUBLE = new JsonBinding<Double>() {
    public Factory<Double> factory() {
      return Factories.DOUBLE;
    }
    public JsonElement toJson(Double value) {
      return new JsonPrimitive(value);
    }
    public Double fromJson(JsonElement json) {
      return json.getAsNumber().doubleValue();
    }
  };

  public static final JsonBinding<String> STRING = new JsonBinding<String>() {
    public Factory<String> factory() {
      return Factories.STRING;
    }
    public JsonElement toJson(String value) {
      return new JsonPrimitive(value);
    }
    public String fromJson(JsonElement json) {
      return json.getAsString();
    }
  };

  public static final JsonBinding<ByteArray> BYTE_ARRAY = new JsonBinding<ByteArray>() {
    public Factory<ByteArray> factory() {
      return Factories.BYTE_ARRAY;
    }
    public JsonElement toJson(ByteArray value) {
      return new JsonPrimitive(new String(Base64.getEncoder().encode(value.getValue())));
    }
    public ByteArray fromJson(JsonElement json) {
      return new ByteArray(Base64.getDecoder().decode(json.getAsString().getBytes()));
    }
  };
  
  public static <T> JsonBinding<ArrayList<T>> arrayList(final JsonBinding<T> factoryT) {
    return new JsonBinding<ArrayList<T>>() {
      public Factory<ArrayList<T>> factory() {
        return Factories.arrayList(factoryT.factory());
      }
      public JsonElement toJson(ArrayList<T> value) {
        JsonArray result = new JsonArray();
        for (T v : value) {
          result.add(factoryT.toJson(v));
        }
        return result;
      }
      public ArrayList<T> fromJson(JsonElement json) {
        ArrayList<T> result = new ArrayList<T>();
        for (JsonElement je : json.getAsJsonArray()) {
          result.add(factoryT.fromJson(je));
        }
        return result;
      }
    };
  }

  public static <T> JsonBinding<HashMap<String,T>> stringMap(final JsonBinding<T> factoryT) {
    return new JsonBinding<HashMap<String,T>>() {
      public Factory<HashMap<String,T>> factory() {
        return Factories.stringMap(factoryT.factory());
      }
      public JsonElement toJson(HashMap<String,T> value) {
        JsonObject result = new JsonObject();
        for (Map.Entry<String,T> e : value.entrySet()) {
          result.add(e.getKey(), factoryT.toJson(e.getValue()));
        }
        return result;
      }
      public HashMap<String,T> fromJson(JsonElement json) {
        JsonObject jobj = json.getAsJsonObject();
        HashMap<String,T> result = new HashMap<String,T>();
        for (Map.Entry<String,JsonElement> e : jobj.entrySet()) {
          result.put(e.getKey(), factoryT.fromJson(e.getValue()));
        }
        return result;
      }
    };
  }
};
