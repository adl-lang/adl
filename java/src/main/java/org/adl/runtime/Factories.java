package org.adl.runtime;

import java.nio.ByteBuffer;
import java.util.ArrayList;

public class Factories
{
  public static final Factory<Void> VoidFactory = new Factory<Void>() {
    public Void create() { return null; }
    public Void create(Void other) { return other; }
  };

  public static final Factory<Boolean> BooleanFactory = new Factory<Boolean>() {
    public Boolean create() { return false; }
    public Boolean create(Boolean other) { return other; }
  };

  public static final Factory<Byte> ByteFactory = new Factory<Byte>() {
    public Byte create() { return 0; }
    public Byte create(Byte other) { return other; }
  };

  public static final Factory<Short> ShortFactory = new Factory<Short>() {
    public Short create() { return 0; }
    public Short create(Short other) { return other; }
  };

  public static final Factory<Integer> IntegerFactory = new Factory<Integer>() {
    public Integer create() { return 0; }
    public Integer create(Integer other) { return other; }
  };

  public static final Factory<Long> LongFactory = new Factory<Long>() {
    public Long create() { return 0L; }
    public Long create(Long other) { return other; }
  };

  public static final Factory<Float> FloatFactory = new Factory<Float>() {
    public Float create() { return 0F; }
    public Float create(Float other) { return other; }
  };

  public static final Factory<Double> DoubleFactory = new Factory<Double>() {
    public Double create() { return 0.0; }
    public Double create(Double other) { return other; }
  };

  public static final Factory<String> StringFactory = new Factory<String>() {
    public String create() { return ""; }
    public String create(String other) { return other; }
  };

  public static final Factory<ByteArray> ByteArrayFactory = new Factory<ByteArray>() {
    public ByteArray create() { return new ByteArray(); }
      public ByteArray create(ByteArray other) { return new ByteArray(other); }
  };

  public static <T> Factory<ArrayList<T>> ArrayListFactory(final Factory<T> factoryT) {
    return new Factory<ArrayList<T>>() {
      public ArrayList<T> create() {
        return new java.util.ArrayList<T>();
      }

      public ArrayList<T> create(ArrayList<T> other) {
        ArrayList<T> result = new ArrayList<T>();
        for (T v : other) {
          result.add( factoryT.create(v) );
        }
        return result;
      }
    };
  }
};
