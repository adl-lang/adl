package adl.test;

import org.adl.runtime.ByteArray;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class S<T> {

  /* Members */

  private Void f_void;
  private boolean f_bool;
  private byte f_int8;
  private short f_int16;
  private int f_int32;
  private long f_int64;
  private byte f_word8;
  private short f_word16;
  private int f_word32;
  private long f_word64;
  private float f_float;
  private double f_double;
  private ByteArray f_bytes;
  private String f_string;
  private java.util.ArrayList<String> f_vstring;
  private A f_a;
  private U f_u;
  private T f_t;
  private B<Short> f_bint16;

  /* Constructors */

  public S(Void f_void, boolean f_bool, byte f_int8, short f_int16, int f_int32, long f_int64, byte f_word8, short f_word16, int f_word32, long f_word64, float f_float, double f_double, ByteArray f_bytes, String f_string, java.util.ArrayList<String> f_vstring, A f_a, U f_u, T f_t, B<Short> f_bint16) {
    this.f_void = f_void;
    this.f_bool = f_bool;
    this.f_int8 = f_int8;
    this.f_int16 = f_int16;
    this.f_int32 = f_int32;
    this.f_int64 = f_int64;
    this.f_word8 = f_word8;
    this.f_word16 = f_word16;
    this.f_word32 = f_word32;
    this.f_word64 = f_word64;
    this.f_float = f_float;
    this.f_double = f_double;
    this.f_bytes = java.util.Objects.requireNonNull(f_bytes);
    this.f_string = java.util.Objects.requireNonNull(f_string);
    this.f_vstring = java.util.Objects.requireNonNull(f_vstring);
    this.f_a = java.util.Objects.requireNonNull(f_a);
    this.f_u = java.util.Objects.requireNonNull(f_u);
    this.f_t = java.util.Objects.requireNonNull(f_t);
    this.f_bint16 = java.util.Objects.requireNonNull(f_bint16);
  }

  /* Accessors and mutators */

  public Void getF_void() {
    return f_void;
  }

  public void setF_void(Void newF_void) {
    f_void = newF_void;
  }

  public boolean getF_bool() {
    return f_bool;
  }

  public void setF_bool(boolean newF_bool) {
    f_bool = newF_bool;
  }

  public byte getF_int8() {
    return f_int8;
  }

  public void setF_int8(byte newF_int8) {
    f_int8 = newF_int8;
  }

  public short getF_int16() {
    return f_int16;
  }

  public void setF_int16(short newF_int16) {
    f_int16 = newF_int16;
  }

  public int getF_int32() {
    return f_int32;
  }

  public void setF_int32(int newF_int32) {
    f_int32 = newF_int32;
  }

  public long getF_int64() {
    return f_int64;
  }

  public void setF_int64(long newF_int64) {
    f_int64 = newF_int64;
  }

  public byte getF_word8() {
    return f_word8;
  }

  public void setF_word8(byte newF_word8) {
    f_word8 = newF_word8;
  }

  public short getF_word16() {
    return f_word16;
  }

  public void setF_word16(short newF_word16) {
    f_word16 = newF_word16;
  }

  public int getF_word32() {
    return f_word32;
  }

  public void setF_word32(int newF_word32) {
    f_word32 = newF_word32;
  }

  public long getF_word64() {
    return f_word64;
  }

  public void setF_word64(long newF_word64) {
    f_word64 = newF_word64;
  }

  public float getF_float() {
    return f_float;
  }

  public void setF_float(float newF_float) {
    f_float = newF_float;
  }

  public double getF_double() {
    return f_double;
  }

  public void setF_double(double newF_double) {
    f_double = newF_double;
  }

  public ByteArray getF_bytes() {
    return f_bytes;
  }

  public void setF_bytes(ByteArray newF_bytes) {
    f_bytes = java.util.Objects.requireNonNull(newF_bytes);
  }

  public String getF_string() {
    return f_string;
  }

  public void setF_string(String newF_string) {
    f_string = java.util.Objects.requireNonNull(newF_string);
  }

  public java.util.ArrayList<String> getF_vstring() {
    return f_vstring;
  }

  public void setF_vstring(java.util.ArrayList<String> newF_vstring) {
    f_vstring = java.util.Objects.requireNonNull(newF_vstring);
  }

  public A getF_a() {
    return f_a;
  }

  public void setF_a(A newF_a) {
    f_a = java.util.Objects.requireNonNull(newF_a);
  }

  public U getF_u() {
    return f_u;
  }

  public void setF_u(U newF_u) {
    f_u = java.util.Objects.requireNonNull(newF_u);
  }

  public T getF_t() {
    return f_t;
  }

  public void setF_t(T newF_t) {
    f_t = java.util.Objects.requireNonNull(newF_t);
  }

  public B<Short> getF_bint16() {
    return f_bint16;
  }

  public void setF_bint16(B<Short> newF_bint16) {
    f_bint16 = java.util.Objects.requireNonNull(newF_bint16);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S)) {
      return false;
    }
    S other = (S)other0;
    return
      f_void.equals(other.f_void) &&
      f_bool == other.f_bool &&
      f_int8 == other.f_int8 &&
      f_int16 == other.f_int16 &&
      f_int32 == other.f_int32 &&
      f_int64 == other.f_int64 &&
      f_word8 == other.f_word8 &&
      f_word16 == other.f_word16 &&
      f_word32 == other.f_word32 &&
      f_word64 == other.f_word64 &&
      f_float == other.f_float &&
      f_double == other.f_double &&
      f_bytes.equals(other.f_bytes) &&
      f_string.equals(other.f_string) &&
      f_vstring.equals(other.f_vstring) &&
      f_a.equals(other.f_a) &&
      f_u.equals(other.f_u) &&
      f_t.equals(other.f_t) &&
      f_bint16.equals(other.f_bint16);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + 0;
    result = result * 37 + (f_bool ? 0 : 1);
    result = result * 37 + (int)f_int8;
    result = result * 37 + (int)f_int16;
    result = result * 37 + f_int32;
    result = result * 37 + (int)(f_int64 ^ (f_int64 >>> 32));
    result = result * 37 + (int)f_word8;
    result = result * 37 + (int)f_word16;
    result = result * 37 + f_word32;
    result = result * 37 + (int)(f_word64 ^ (f_word64 >>> 32));
    result = result * 37 + Float.valueOf(f_float).hashCode();
    result = result * 37 + Double.valueOf(f_double).hashCode();
    result = result * 37 + f_bytes.hashCode();
    result = result * 37 + f_string.hashCode();
    result = result * 37 + f_vstring.hashCode();
    result = result * 37 + f_a.hashCode();
    result = result * 37 + f_u.hashCode();
    result = result * 37 + f_t.hashCode();
    result = result * 37 + f_bint16.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<S<T>> factory(Factory<T> factoryT) {
    return new Factory<S<T>>() {
      final Factory<ByteArray> f_bytes = Factories.ByteArrayFactory;
      final Factory<java.util.ArrayList<String>> f_vstring = Factories.ArrayListFactory(Factories.StringFactory);
      final Factory<A> f_a = A.factory;
      final Factory<U> f_u = U.factory;
      final Factory<T> f_t = factoryT;
      final Factory<B<Short>> f_bint16 = B.factory(Factories.ShortFactory);

      public S<T> create() {
        return new S<T>(null, true, (byte)-5, (short)-10000, 56, 40000L, (byte)32, (short)50000, 124456, 2344L, 0.5F, 0.45, f_bytes.create(), "abcd", f_vstring.create(), f_a.create(), f_u.create(), f_t.create(), f_bint16.create());
      }

      public S<T> create(S<T> other) {
        return new S<T>(other.getF_void(), other.getF_bool(), other.getF_int8(), other.getF_int16(), other.getF_int32(), other.getF_int64(), other.getF_word8(), other.getF_word16(), other.getF_word32(), other.getF_word64(), other.getF_float(), other.getF_double(), f_bytes.create(other.getF_bytes()), other.getF_string(), f_vstring.create(other.getF_vstring()), f_a.create(other.getF_a()), f_u.create(other.getF_u()), f_t.create(other.getF_t()), f_bint16.create(other.getF_bint16()));
      }
    };
  }
}