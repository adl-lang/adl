package org.adl.runtime;

import java.nio.ByteBuffer;
import java.util.ArrayList;

/**
 * Useful standard implementations of the Factory interface.
 */
public class Factories {
  public static final Factory<Void> VOID = new Factory<Void>() {
    public Void create() { return null; }
    public Void create(Void other) { return other; }
  };

  public static final Factory<Boolean> BOOLEAN = new Factory<Boolean>() {
    public Boolean create() { return false; }
    public Boolean create(Boolean other) { return other; }
  };

  public static final Factory<Byte> BYTE = new Factory<Byte>() {
    public Byte create() { return 0; }
    public Byte create(Byte other) { return other; }
  };

  public static final Factory<Short> SHORT = new Factory<Short>() {
    public Short create() { return 0; }
    public Short create(Short other) { return other; }
  };

  public static final Factory<Integer> INTEGER = new Factory<Integer>() {
    public Integer create() { return 0; }
    public Integer create(Integer other) { return other; }
  };

  public static final Factory<Long> LONG = new Factory<Long>() {
    public Long create() { return 0L; }
    public Long create(Long other) { return other; }
  };

  public static final Factory<Float> FLOAT = new Factory<Float>() {
    public Float create() { return 0F; }
    public Float create(Float other) { return other; }
  };

  public static final Factory<Double> DOUBLE = new Factory<Double>() {
    public Double create() { return 0.0; }
    public Double create(Double other) { return other; }
  };

  public static final Factory<String> STRING = new Factory<String>() {
    public String create() { return ""; }
    public String create(String other) { return other; }
  };

  public static final Factory<ByteArray> BYTE_ARRAY = new Factory<ByteArray>() {
    public ByteArray create() { return new ByteArray(); }
      public ByteArray create(ByteArray other) { return new ByteArray(other); }
  };

  public static <T> Factory<ArrayList<T>> arrayList(final Factory<T> factoryT) {
    return new Factory<ArrayList<T>>() {
      public ArrayList<T> create() {
        return new java.util.ArrayList<T>();
      }

      public ArrayList<T> create(ArrayList<T> other) {
        ArrayList<T> result = new ArrayList<T>();
        for (T v : other) {
          result.add(factoryT.create(v));
        }
        return result;
      }
    };
  }

  @SafeVarargs
  public static <T> ArrayList<T> arrayList(T... values) {
    ArrayList<T> result  = new ArrayList<T>();
    for (int i=  0; i < values.length; i++) {
      result.add(values[i]);
    }
    return result;
  }
  
};
