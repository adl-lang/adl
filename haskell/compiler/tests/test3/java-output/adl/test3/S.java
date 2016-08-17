package adl.test3;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.ByteArray;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.ArrayList;
import java.util.Objects;

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
  private ArrayList<String> f_vstring;
  private A f_a;
  private U f_u;
  private T f_t;
  private B<Short> f_bint16;

  /* Constructors */

  public S(Void f_void, boolean f_bool, byte f_int8, short f_int16, int f_int32, long f_int64, byte f_word8, short f_word16, int f_word32, long f_word64, float f_float, double f_double, ByteArray f_bytes, String f_string, ArrayList<String> f_vstring, A f_a, U f_u, T f_t, B<Short> f_bint16) {
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
    this.f_bytes = Objects.requireNonNull(f_bytes);
    this.f_string = Objects.requireNonNull(f_string);
    this.f_vstring = Objects.requireNonNull(f_vstring);
    this.f_a = Objects.requireNonNull(f_a);
    this.f_u = Objects.requireNonNull(f_u);
    this.f_t = Objects.requireNonNull(f_t);
    this.f_bint16 = Objects.requireNonNull(f_bint16);
  }

  /* Accessors and mutators */

  public Void getF_void() {
    return f_void;
  }

  public void setF_void(Void f_void) {
    this.f_void = f_void;
  }

  public boolean getF_bool() {
    return f_bool;
  }

  public void setF_bool(boolean f_bool) {
    this.f_bool = f_bool;
  }

  public byte getF_int8() {
    return f_int8;
  }

  public void setF_int8(byte f_int8) {
    this.f_int8 = f_int8;
  }

  public short getF_int16() {
    return f_int16;
  }

  public void setF_int16(short f_int16) {
    this.f_int16 = f_int16;
  }

  public int getF_int32() {
    return f_int32;
  }

  public void setF_int32(int f_int32) {
    this.f_int32 = f_int32;
  }

  public long getF_int64() {
    return f_int64;
  }

  public void setF_int64(long f_int64) {
    this.f_int64 = f_int64;
  }

  public byte getF_word8() {
    return f_word8;
  }

  public void setF_word8(byte f_word8) {
    this.f_word8 = f_word8;
  }

  public short getF_word16() {
    return f_word16;
  }

  public void setF_word16(short f_word16) {
    this.f_word16 = f_word16;
  }

  public int getF_word32() {
    return f_word32;
  }

  public void setF_word32(int f_word32) {
    this.f_word32 = f_word32;
  }

  public long getF_word64() {
    return f_word64;
  }

  public void setF_word64(long f_word64) {
    this.f_word64 = f_word64;
  }

  public float getF_float() {
    return f_float;
  }

  public void setF_float(float f_float) {
    this.f_float = f_float;
  }

  public double getF_double() {
    return f_double;
  }

  public void setF_double(double f_double) {
    this.f_double = f_double;
  }

  public ByteArray getF_bytes() {
    return f_bytes;
  }

  public void setF_bytes(ByteArray f_bytes) {
    this.f_bytes = Objects.requireNonNull(f_bytes);
  }

  public String getF_string() {
    return f_string;
  }

  public void setF_string(String f_string) {
    this.f_string = Objects.requireNonNull(f_string);
  }

  public ArrayList<String> getF_vstring() {
    return f_vstring;
  }

  public void setF_vstring(ArrayList<String> f_vstring) {
    this.f_vstring = Objects.requireNonNull(f_vstring);
  }

  public A getF_a() {
    return f_a;
  }

  public void setF_a(A f_a) {
    this.f_a = Objects.requireNonNull(f_a);
  }

  public U getF_u() {
    return f_u;
  }

  public void setF_u(U f_u) {
    this.f_u = Objects.requireNonNull(f_u);
  }

  public T getF_t() {
    return f_t;
  }

  public void setF_t(T f_t) {
    this.f_t = Objects.requireNonNull(f_t);
  }

  public B<Short> getF_bint16() {
    return f_bint16;
  }

  public void setF_bint16(B<Short> f_bint16) {
    this.f_bint16 = Objects.requireNonNull(f_bint16);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S)) {
      return false;
    }
    S other = (S) other0;
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
    result = result * 37 + (int) f_int8;
    result = result * 37 + (int) f_int16;
    result = result * 37 + f_int32;
    result = result * 37 + (int) (f_int64 ^ (f_int64 >>> 32));
    result = result * 37 + (int) f_word8;
    result = result * 37 + (int) f_word16;
    result = result * 37 + f_word32;
    result = result * 37 + (int) (f_word64 ^ (f_word64 >>> 32));
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
      final Factory<Void> f_void = Factories.VOID;
      final Factory<Boolean> f_bool = Factories.BOOLEAN;
      final Factory<Byte> f_int8 = Factories.BYTE;
      final Factory<Short> f_int16 = Factories.SHORT;
      final Factory<Integer> f_int32 = Factories.INTEGER;
      final Factory<Long> f_int64 = Factories.LONG;
      final Factory<Byte> f_word8 = Factories.BYTE;
      final Factory<Short> f_word16 = Factories.SHORT;
      final Factory<Integer> f_word32 = Factories.INTEGER;
      final Factory<Long> f_word64 = Factories.LONG;
      final Factory<Float> f_float = Factories.FLOAT;
      final Factory<Double> f_double = Factories.DOUBLE;
      final Factory<ByteArray> f_bytes = Factories.BYTE_ARRAY;
      final Factory<String> f_string = Factories.STRING;
      final Factory<ArrayList<String>> f_vstring = Factories.arrayList(Factories.STRING);
      final Factory<A> f_a = A.FACTORY;
      final Factory<U> f_u = U.FACTORY;
      final Factory<T> f_t = factoryT;
      final Factory<B<Short>> f_bint16 = B.factory(Factories.SHORT);

      public S<T> create() {
        return new S<T>(
          null,
          true,
          (byte)-5,
          (short)-10000,
          56,
          40000L,
          (byte)32,
          (short)50000,
          124456,
          2344L,
          0.5F,
          0.45,
          new ByteArray("hello".getBytes()),
          "abcd",
          Factories.arrayList("xy", "ab"),
          new A((short)0, "xyz", true),
          U.f_int((short)45),
          f_t.create(),
          new B<Short>((short)56, "yikes", Factories.arrayList((short)1, (short)2, (short)3), new XY<Short>((short)5, (short)5))
          );
      }

      public S<T> create(S<T> other) {
        return new S<T>(
          other.getF_void(),
          other.getF_bool(),
          other.getF_int8(),
          other.getF_int16(),
          other.getF_int32(),
          other.getF_int64(),
          other.getF_word8(),
          other.getF_word16(),
          other.getF_word32(),
          other.getF_word64(),
          other.getF_float(),
          other.getF_double(),
          f_bytes.create(other.getF_bytes()),
          other.getF_string(),
          f_vstring.create(other.getF_vstring()),
          f_a.create(other.getF_a()),
          f_u.create(other.getF_u()),
          f_t.create(other.getF_t()),
          f_bint16.create(other.getF_bint16())
          );
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<S<T>> jsonBinding(JsonBinding<T> bindingT) {
    final JsonBinding<Void> f_void = JsonBindings.VOID;
    final JsonBinding<Boolean> f_bool = JsonBindings.BOOLEAN;
    final JsonBinding<Byte> f_int8 = JsonBindings.BYTE;
    final JsonBinding<Short> f_int16 = JsonBindings.SHORT;
    final JsonBinding<Integer> f_int32 = JsonBindings.INTEGER;
    final JsonBinding<Long> f_int64 = JsonBindings.LONG;
    final JsonBinding<Byte> f_word8 = JsonBindings.BYTE;
    final JsonBinding<Short> f_word16 = JsonBindings.SHORT;
    final JsonBinding<Integer> f_word32 = JsonBindings.INTEGER;
    final JsonBinding<Long> f_word64 = JsonBindings.LONG;
    final JsonBinding<Float> f_float = JsonBindings.FLOAT;
    final JsonBinding<Double> f_double = JsonBindings.DOUBLE;
    final JsonBinding<ByteArray> f_bytes = JsonBindings.BYTE_ARRAY;
    final JsonBinding<String> f_string = JsonBindings.STRING;
    final JsonBinding<ArrayList<String>> f_vstring = JsonBindings.arrayList(JsonBindings.STRING);
    final JsonBinding<A> f_a = A.jsonBinding();
    final JsonBinding<U> f_u = U.jsonBinding();
    final JsonBinding<T> f_t = bindingT;
    final JsonBinding<B<Short>> f_bint16 = B.jsonBinding(JsonBindings.SHORT);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<S<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<S<T>>() {
      public Factory<S<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(S<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("f_void", f_void.toJson(_value.f_void));
        _result.add("f_bool", f_bool.toJson(_value.f_bool));
        _result.add("f_int8", f_int8.toJson(_value.f_int8));
        _result.add("f_int16", f_int16.toJson(_value.f_int16));
        _result.add("f_int32", f_int32.toJson(_value.f_int32));
        _result.add("f_int64", f_int64.toJson(_value.f_int64));
        _result.add("f_word8", f_word8.toJson(_value.f_word8));
        _result.add("f_word16", f_word16.toJson(_value.f_word16));
        _result.add("f_word32", f_word32.toJson(_value.f_word32));
        _result.add("f_word64", f_word64.toJson(_value.f_word64));
        _result.add("f_float", f_float.toJson(_value.f_float));
        _result.add("f_double", f_double.toJson(_value.f_double));
        _result.add("f_bytes", f_bytes.toJson(_value.f_bytes));
        _result.add("f_string", f_string.toJson(_value.f_string));
        _result.add("f_vstring", f_vstring.toJson(_value.f_vstring));
        _result.add("f_a", f_a.toJson(_value.f_a));
        _result.add("f_u", f_u.toJson(_value.f_u));
        _result.add("f_t", f_t.toJson(_value.f_t));
        _result.add("f_bint16", f_bint16.toJson(_value.f_bint16));
        return _result;
      }

      public S<T> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new S<T>(
          _obj.has("f_void") ? f_void.fromJson(_obj.get("f_void")) : null,
          _obj.has("f_bool") ? f_bool.fromJson(_obj.get("f_bool")) : true,
          _obj.has("f_int8") ? f_int8.fromJson(_obj.get("f_int8")) : (byte)-5,
          _obj.has("f_int16") ? f_int16.fromJson(_obj.get("f_int16")) : (short)-10000,
          _obj.has("f_int32") ? f_int32.fromJson(_obj.get("f_int32")) : 56,
          _obj.has("f_int64") ? f_int64.fromJson(_obj.get("f_int64")) : 40000L,
          _obj.has("f_word8") ? f_word8.fromJson(_obj.get("f_word8")) : (byte)32,
          _obj.has("f_word16") ? f_word16.fromJson(_obj.get("f_word16")) : (short)50000,
          _obj.has("f_word32") ? f_word32.fromJson(_obj.get("f_word32")) : 124456,
          _obj.has("f_word64") ? f_word64.fromJson(_obj.get("f_word64")) : 2344L,
          _obj.has("f_float") ? f_float.fromJson(_obj.get("f_float")) : 0.5F,
          _obj.has("f_double") ? f_double.fromJson(_obj.get("f_double")) : 0.45,
          _obj.has("f_bytes") ? f_bytes.fromJson(_obj.get("f_bytes")) : new ByteArray("hello".getBytes()),
          _obj.has("f_string") ? f_string.fromJson(_obj.get("f_string")) : "abcd",
          _obj.has("f_vstring") ? f_vstring.fromJson(_obj.get("f_vstring")) : Factories.arrayList("xy", "ab"),
          _obj.has("f_a") ? f_a.fromJson(_obj.get("f_a")) : new A((short)0, "xyz", true),
          _obj.has("f_u") ? f_u.fromJson(_obj.get("f_u")) : U.f_int((short)45),
          _obj.has("f_t") ? f_t.fromJson(_obj.get("f_t")) : factoryT.create(),
          _obj.has("f_bint16") ? f_bint16.fromJson(_obj.get("f_bint16")) : new B<Short>((short)56, "yikes", Factories.arrayList((short)1, (short)2, (short)3), new XY<Short>((short)5, (short)5))
        );
      }
    };
  }
}
