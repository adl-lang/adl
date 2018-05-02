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
  };

  public static final Factory<Boolean> BOOLEAN = new Factory<Boolean>() {
    @Override
    public Boolean create() { return false; }
    @Override
    public Boolean create(Boolean other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Boolean"); }
  };

  public static final Factory<Byte> INT8 = new Factory<Byte>() {
    @Override
    public Byte create() { return 0; }
    @Override
    public Byte create(Byte other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Int8"); }
  };

  public static final Factory<Short> INT16 = new Factory<Short>() {
    @Override
    public Short create() { return 0; }
    @Override
    public Short create(Short other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Int16"); }
  };

  public static final Factory<Integer> INT32 = new Factory<Integer>() {
    @Override
    public Integer create() { return 0; }
    @Override
    public Integer create(Integer other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Int32"); }
  };

  public static final Factory<Long> INT64 = new Factory<Long>() {
    @Override
    public Long create() { return 0L; }
    @Override
    public Long create(Long other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Int64"); }
  };

  public static final Factory<Byte> WORD8 = new Factory<Byte>() {
    @Override
    public Byte create() { return 0; }
    @Override
    public Byte create(Byte other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Word8"); }
  };

  public static final Factory<Short> WORD16 = new Factory<Short>() {
    @Override
    public Short create() { return 0; }
    @Override
    public Short create(Short other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Word16"); }
  };

  public static final Factory<Integer> WORD32 = new Factory<Integer>() {
    @Override
    public Integer create() { return 0; }
    @Override
    public Integer create(Integer other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Word32"); }
  };

  public static final Factory<Long> WORD64 = new Factory<Long>() {
    @Override
    public Long create() { return 0L; }
    @Override
    public Long create(Long other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Word64"); }
  };

  public static final Factory<Float> FLOAT = new Factory<Float>() {
    @Override
    public Float create() { return 0F; }
    @Override
    public Float create(Float other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Float"); }
  };

  public static final Factory<Double> DOUBLE = new Factory<Double>() {
    @Override
    public Double create() { return 0.0; }
    @Override
    public Double create(Double other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Double"); }
  };

  public static final Factory<String> STRING = new Factory<String>() {
    @Override
    public String create() { return ""; }
    @Override
    public String create(String other) { return other; }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("String"); }
  };

  public static final Factory<ByteArray> BYTE_ARRAY = new Factory<ByteArray>() {
    @Override
    public ByteArray create() { return new ByteArray(); }
    @Override
    public ByteArray create(ByteArray other) { return new ByteArray(other); }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("ByteVector"); }
  };

  public static final Factory<JsonElement> JSON = new Factory<JsonElement>() {
    @Override
    public JsonElement create() { return JsonNull.INSTANCE; }
    @Override
    public JsonElement create(JsonElement other) { return other.deepCopy(); }
    @Override
    public TypeExpr typeExpr() { return primTypeExpr("Json"); }
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
