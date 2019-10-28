package org.adl.runtime;

import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;

import com.google.gson.JsonElement;
import com.google.gson.JsonNull;

import java.util.ArrayList;
import java.util.Optional;
import java.util.Map;
import java.util.HashMap;

/**
 * Useful standard implementations of the Factory interface.
 */
public class Factories {
  public static final Factory<Void> VOID = new Factory<Void>() {
    @Override
    public Void create() { return null; }
    @Override
    public Void create(Void other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Void"); }
    @Override
    public JsonBinding<Void> jsonBinding() { return JsonBindings.VOID; }
  };

  public static final Factory<Boolean> BOOLEAN = new Factory<Boolean>() {
    @Override
    public Boolean create() { return false; }
    @Override
    public Boolean create(Boolean other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Boolean"); }
    @Override
    public JsonBinding<Boolean> jsonBinding() { return JsonBindings.BOOLEAN; }
  };

  public static final Factory<Byte> INT8 = new Factory<Byte>() {
    @Override
    public Byte create() { return 0; }
    @Override
    public Byte create(Byte other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Int8"); }
    @Override
    public JsonBinding<Byte> jsonBinding() { return JsonBindings.INT8; }
  };

  public static final Factory<Short> INT16 = new Factory<Short>() {
    @Override
    public Short create() { return 0; }
    @Override
    public Short create(Short other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Int16"); }
    @Override
    public JsonBinding<Short> jsonBinding() { return JsonBindings.INT16; }
  };

  public static final Factory<Integer> INT32 = new Factory<Integer>() {
    @Override
    public Integer create() { return 0; }
    @Override
    public Integer create(Integer other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Int32"); }
    @Override
    public JsonBinding<Integer> jsonBinding() { return JsonBindings.INT32; }
  };

  public static final Factory<Long> INT64 = new Factory<Long>() {
    @Override
    public Long create() { return 0L; }
    @Override
    public Long create(Long other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Int64"); }
    @Override
    public JsonBinding<Long> jsonBinding() { return JsonBindings.INT64; }
  };

  public static final Factory<Byte> WORD8 = new Factory<Byte>() {
    @Override
    public Byte create() { return 0; }
    @Override
    public Byte create(Byte other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Word8"); }
    @Override
    public JsonBinding<Byte> jsonBinding() { return JsonBindings.WORD8; }
  };

  public static final Factory<Short> WORD16 = new Factory<Short>() {
    @Override
    public Short create() { return 0; }
    @Override
    public Short create(Short other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Word16"); }
    @Override
    public JsonBinding<Short> jsonBinding() { return JsonBindings.WORD16; }
  };

  public static final Factory<Integer> WORD32 = new Factory<Integer>() {
    @Override
    public Integer create() { return 0; }
    @Override
    public Integer create(Integer other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Word32"); }
    @Override
    public JsonBinding<Integer> jsonBinding() { return JsonBindings.WORD32; }
  };

  public static final Factory<Long> WORD64 = new Factory<Long>() {
    @Override
    public Long create() { return 0L; }
    @Override
    public Long create(Long other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Word64"); }
    @Override
    public JsonBinding<Long> jsonBinding() { return JsonBindings.WORD64; }
  };

  public static final Factory<Float> FLOAT = new Factory<Float>() {
    @Override
    public Float create() { return 0F; }
    @Override
    public Float create(Float other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Float"); }
    @Override
    public JsonBinding<Float> jsonBinding() { return JsonBindings.FLOAT; }
  };

  public static final Factory<Double> DOUBLE = new Factory<Double>() {
    @Override
    public Double create() { return 0.0; }
    @Override
    public Double create(Double other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Double"); }
    @Override
    public JsonBinding<Double> jsonBinding() { return JsonBindings.DOUBLE; }
  };

  public static final Factory<String> STRING = new Factory<String>() {
    @Override
    public String create() { return ""; }
    @Override
    public String create(String other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("String"); }
    @Override
    public JsonBinding<String> jsonBinding() { return JsonBindings.STRING; }
  };

  public static final Factory<ByteArray> BYTE_ARRAY = new Factory<ByteArray>() {
    @Override
    public ByteArray create() { return new ByteArray(); }
    @Override
    public ByteArray create(ByteArray other) { return new ByteArray(other); }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("ByteVector"); }
    @Override
    public JsonBinding<ByteArray> jsonBinding() { return JsonBindings.BYTE_ARRAY; }
  };

  public static final Factory<JsonElement> JSON = new Factory<JsonElement>() {
    @Override
    public JsonElement create() { return JsonNull.INSTANCE; }
    @Override
    public JsonElement create(JsonElement other) { return other.deepCopy(); }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Json"); }
    @Override
    public JsonBinding<JsonElement> jsonBinding() { return JsonBindings.JSON; }
  };

  public static <T> Factory<ArrayList<T>> arrayList(final Factory<T> factoryT) {
    return new Factory<ArrayList<T>>() {
      @Override
      public ArrayList<T> create() {
        return new java.util.ArrayList<>();
      }

      @Override
      public ArrayList<T> create(ArrayList<T> other) {
        ArrayList<T> result = new ArrayList<>();
        for (T v : other) {
          result.add(factoryT.create(v));
        }
        return result;
      }

      @Override
      public TypeExpr typeExpr() {
        return primTypeExpr("Vector", factoryT.typeExpr());
      }
      @Override
      public JsonBinding<ArrayList<T>> jsonBinding() { return JsonBindings.arrayList(factoryT.jsonBinding()); }
    };
  }

  @SafeVarargs
  public static <T> ArrayList<T> arrayList(T... values) {
    ArrayList<T> result  = new ArrayList<>();
    for (int i=  0; i < values.length; i++) {
      result.add(values[i]);
    }
    return result;
  }

  public static <T> Factory<HashMap<String,T>> stringMap(final Factory<T> factoryT) {
    return new Factory<HashMap<String,T>>() {
      @Override
      public HashMap<String,T> create() {
        return new java.util.HashMap<>();
      }

      @Override
      public HashMap<String,T> create(HashMap<String,T> other) {
        HashMap<String,T> result = new HashMap<>();
        for (Map.Entry<String,T> e : other.entrySet()) {
          result.put(e.getKey(), factoryT.create(e.getValue()));
        }
        return result;
      }

      @Override
      public TypeExpr typeExpr() {
        return primTypeExpr("StringMap", factoryT.typeExpr());
      }
      @Override
      public JsonBinding<HashMap<String,T>> jsonBinding() {
        return JsonBindings.stringMap(factoryT.jsonBinding());
      }
    };
  }

  public static <T> Factory<Optional<T>> nullable(final Factory<T> factoryT) {
    return new Factory<Optional<T>>() {
      @Override
      public Optional<T> create() {
        return java.util.Optional.<T>empty();
      }

      @Override
      public Optional<T> create(Optional<T> other) {
        return other.map(factoryT::create);
      }

      @Override
      public TypeExpr typeExpr() {
        return primTypeExpr("Nullable", factoryT.typeExpr());
      }
      @Override
      public JsonBinding<Optional<T>> jsonBinding() {
        return JsonBindings.nullable(factoryT.jsonBinding());
      }
    };
  }

  public static <T> Factory<TypeToken<T>> typeProxy(final Factory<T> factoryT) {
    return new Factory<TypeToken<T>>() {
      @Override
      public TypeToken<T> create() {
        return new TypeToken<T>(factoryT.jsonBinding());
      }

      @Override
      public TypeToken<T> create(TypeToken<T> other) {
        return other;
      }

      @Override
      public TypeExpr typeExpr() {
        return primTypeExpr("TypeToken", factoryT.typeExpr());
      }
      @Override
      public JsonBinding<TypeToken<T>> jsonBinding() {
        return JsonBindings.typeProxy(factoryT.jsonBinding());
      }
    };
  }

  public static <T> HashMap<String,T> stringMap() {
    return new HashMap<>();
  }

  @SuppressWarnings("unchecked")
  public static <T> HashMap<String,T> stringMap(String k1, T v1, Object... kvs) {
    HashMap<String,T> result = new HashMap<>();
    result.put(k1, v1);
    for (int i = 0; i < kvs.length;) {
      String k = (String) kvs[i++];
      T v = (T) kvs[i++];
      result.put(k, v);
    }
    return result;
  }

  private static TypeExpr primTypeExpr(String primitive) {
    return new TypeExpr(TypeRef.primitive(primitive), new ArrayList<>());
  }

  private static TypeExpr primTypeExpr(String primitive, TypeExpr param1) {
    ArrayList<TypeExpr> params = new ArrayList<>();
    params.add(param1);
    return new TypeExpr(TypeRef.primitive(primitive), params);
  }

};
