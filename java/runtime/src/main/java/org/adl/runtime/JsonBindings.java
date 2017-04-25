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
import java.util.Set;

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
      if (json.isJsonNull()) {
        return null;
      }
      throw new JsonParseException("expected null");
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
      try {
        return json.getAsBoolean();
      } catch (UnsupportedOperationException | ClassCastException e) {
        throw new JsonParseException("expected a boolean");
      }
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
      try {
        return json.getAsNumber().byteValue();
      } catch (UnsupportedOperationException | ClassCastException e) {
        throw new JsonParseException("expected a number");
      }
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
      try {
        return json.getAsNumber().shortValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
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
      try {
        return json.getAsNumber().intValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
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
      try {
        return json.getAsNumber().longValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
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
      try {
        return json.getAsNumber().floatValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
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
      try {
        return json.getAsNumber().doubleValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
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
      if (json.isJsonPrimitive() && json.getAsJsonPrimitive().isString()) {
        return json.getAsString();
      }
        
      throw new JsonParseException("expected a string");
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
      try {
        return new ByteArray(Base64.getDecoder().decode(json.getAsString().getBytes()));
      } catch (UnsupportedOperationException | ClassCastException e) {
        throw new JsonParseException("expected a base 64 encoded string");
      } catch (RuntimeException e) {
        throw new JsonParseException("expected a base 64 encoded string");
      }
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
        JsonArray jarray;
        try {
          jarray = json.getAsJsonArray();
        } catch (UnsupportedOperationException | ClassCastException e) {
          throw new JsonParseException("expected an array");
        }
        int i = 0;
        for (JsonElement je : jarray) {
          try {
            result.add(factoryT.fromJson(je));
          } catch (JsonParseException e) {
            e.pushIndex(i);
            throw e;
          }
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
        JsonObject jobj;
        try {
          jobj = json.getAsJsonObject();
        } catch (UnsupportedOperationException | ClassCastException e) {
          throw new JsonParseException("expected an object");
        }
        HashMap<String,T> result = new HashMap<String,T>();
        for (Map.Entry<String,JsonElement> e : jobj.entrySet()) {
          try {
            result.put(e.getKey(), factoryT.fromJson(e.getValue()));
          } catch (JsonParseException jpe) {
            jpe.pushField(e.getKey());
            throw jpe;
          }
        }
        return result;
      }
    };
  }

  public static <T> JsonElement unionToJson(String name, T value, JsonBinding<T> jsonBinding) {
    if(value == null) {
      return new JsonPrimitive(name);
    } else {
      JsonObject result = new JsonObject();
      result.add(name, jsonBinding.toJson(value));
      return result;
    }
  }

  public static String unionNameFromJson(JsonElement json) {
    if(json.isJsonObject()) {
      Set<Map.Entry<String,JsonElement>> entries = json.getAsJsonObject().entrySet();
      if (entries.size() != 1) {
        throw new IllegalStateException();
      }
      for (Map.Entry<String,JsonElement> v : entries) {
        return v.getKey();
      }
      throw new IllegalStateException();
    } else {
      try {
        return json.getAsString();
      } catch (UnsupportedOperationException | ClassCastException e) {
        throw new JsonParseException( "expected an object or string");
      }
    }
  }

  public static <T> T unionValueFromJson(JsonElement json, JsonBinding<T> jsonBinding) {
    if(json.isJsonObject()) {
      Set<Map.Entry<String,JsonElement>> entries = json.getAsJsonObject().entrySet();
      if (entries.size() != 1) {
        throw new IllegalStateException();
      }
      for (Map.Entry<String,JsonElement> v : entries) {
        try {
          return jsonBinding.fromJson(v.getValue());
        } catch (JsonParseException e) {
          e.pushField(v.getKey());
          throw e;
        }
      }
      throw new IllegalStateException();
    } else {
      return null;
    }
  }

  public static JsonObject objectFromJson(JsonElement json) {
    if (!json.isJsonObject()) {
      throw new JsonParseException("expected an object");
    }
    return json.getAsJsonObject();
  }
  
  public static <T> T fieldFromJson(JsonObject obj, String field, JsonBinding<T> jsonBinding) {
    JsonElement el = objectFromJson(obj).get(field);
    if (el == null) {
      throw new JsonParseException( "expected field " + field);
    }
    try {
      return jsonBinding.fromJson(el);
    } catch(JsonParseException e) {
      e.pushField(field);
      throw e;
    }
  }
};
