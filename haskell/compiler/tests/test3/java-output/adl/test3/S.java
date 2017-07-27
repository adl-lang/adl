/* Code generated from adl module test3 */

package adl.test3;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.ByteArray;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonHelpers;
import org.adl.runtime.Lazy;
import org.adl.sys.adlast.ScopedName;
import org.adl.sys.adlast.TypeExpr;
import org.adl.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.HashMap;
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
  private U f_u1;
  private E f_e;
  private T f_t;
  private B<Short> f_bint16;
  private HashMap<String, Integer> f_smap;
  private JsonElement f_json1;
  private JsonElement f_json2;

  /* Constructors */

  public S(Void f_void, boolean f_bool, byte f_int8, short f_int16, int f_int32, long f_int64, byte f_word8, short f_word16, int f_word32, long f_word64, float f_float, double f_double, ByteArray f_bytes, String f_string, ArrayList<String> f_vstring, A f_a, U f_u, U f_u1, E f_e, T f_t, B<Short> f_bint16, HashMap<String, Integer> f_smap, JsonElement f_json1, JsonElement f_json2) {
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
    this.f_u1 = Objects.requireNonNull(f_u1);
    this.f_e = Objects.requireNonNull(f_e);
    this.f_t = Objects.requireNonNull(f_t);
    this.f_bint16 = Objects.requireNonNull(f_bint16);
    this.f_smap = Objects.requireNonNull(f_smap);
    this.f_json1 = Objects.requireNonNull(f_json1);
    this.f_json2 = Objects.requireNonNull(f_json2);
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

  public U getF_u1() {
    return f_u1;
  }

  public void setF_u1(U f_u1) {
    this.f_u1 = Objects.requireNonNull(f_u1);
  }

  public E getF_e() {
    return f_e;
  }

  public void setF_e(E f_e) {
    this.f_e = Objects.requireNonNull(f_e);
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

  public HashMap<String, Integer> getF_smap() {
    return f_smap;
  }

  public void setF_smap(HashMap<String, Integer> f_smap) {
    this.f_smap = Objects.requireNonNull(f_smap);
  }

  public JsonElement getF_json1() {
    return f_json1;
  }

  public void setF_json1(JsonElement f_json1) {
    this.f_json1 = Objects.requireNonNull(f_json1);
  }

  public JsonElement getF_json2() {
    return f_json2;
  }

  public void setF_json2(JsonElement f_json2) {
    this.f_json2 = Objects.requireNonNull(f_json2);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S)) {
      return false;
    }
    S<?> other = (S<?>) other0;
    return
      true &&
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
      f_u1.equals(other.f_u1) &&
      f_e.equals(other.f_e) &&
      f_t.equals(other.f_t) &&
      f_bint16.equals(other.f_bint16) &&
      f_smap.equals(other.f_smap) &&
      f_json1.equals(other.f_json1) &&
      f_json2.equals(other.f_json2);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + 0;
    _result = _result * 37 + (f_bool ? 0 : 1);
    _result = _result * 37 + (int) f_int8;
    _result = _result * 37 + (int) f_int16;
    _result = _result * 37 + f_int32;
    _result = _result * 37 + (int) (f_int64 ^ (f_int64 >>> 32));
    _result = _result * 37 + (int) f_word8;
    _result = _result * 37 + (int) f_word16;
    _result = _result * 37 + f_word32;
    _result = _result * 37 + (int) (f_word64 ^ (f_word64 >>> 32));
    _result = _result * 37 + Float.valueOf(f_float).hashCode();
    _result = _result * 37 + Double.valueOf(f_double).hashCode();
    _result = _result * 37 + f_bytes.hashCode();
    _result = _result * 37 + f_string.hashCode();
    _result = _result * 37 + f_vstring.hashCode();
    _result = _result * 37 + f_a.hashCode();
    _result = _result * 37 + f_u.hashCode();
    _result = _result * 37 + f_u1.hashCode();
    _result = _result * 37 + f_e.hashCode();
    _result = _result * 37 + f_t.hashCode();
    _result = _result * 37 + f_bint16.hashCode();
    _result = _result * 37 + f_smap.hashCode();
    _result = _result * 37 + f_json1.hashCode();
    _result = _result * 37 + f_json2.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<S<T>> factory(Factory<T> factoryT) {
    return new Factory<S<T>>() {
      final Lazy<Factory<Void>> f_void = new Lazy<>(() -> Factories.VOID);
      final Lazy<Factory<Boolean>> f_bool = new Lazy<>(() -> Factories.BOOLEAN);
      final Lazy<Factory<Byte>> f_int8 = new Lazy<>(() -> Factories.INT8);
      final Lazy<Factory<Short>> f_int16 = new Lazy<>(() -> Factories.INT16);
      final Lazy<Factory<Integer>> f_int32 = new Lazy<>(() -> Factories.INT32);
      final Lazy<Factory<Long>> f_int64 = new Lazy<>(() -> Factories.INT64);
      final Lazy<Factory<Byte>> f_word8 = new Lazy<>(() -> Factories.WORD8);
      final Lazy<Factory<Short>> f_word16 = new Lazy<>(() -> Factories.WORD16);
      final Lazy<Factory<Integer>> f_word32 = new Lazy<>(() -> Factories.WORD32);
      final Lazy<Factory<Long>> f_word64 = new Lazy<>(() -> Factories.WORD64);
      final Lazy<Factory<Float>> f_float = new Lazy<>(() -> Factories.FLOAT);
      final Lazy<Factory<Double>> f_double = new Lazy<>(() -> Factories.DOUBLE);
      final Lazy<Factory<ByteArray>> f_bytes = new Lazy<>(() -> Factories.BYTE_ARRAY);
      final Lazy<Factory<String>> f_string = new Lazy<>(() -> Factories.STRING);
      final Lazy<Factory<ArrayList<String>>> f_vstring = new Lazy<>(() -> Factories.arrayList(Factories.STRING));
      final Lazy<Factory<A>> f_a = new Lazy<>(() -> A.FACTORY);
      final Lazy<Factory<U>> f_u = new Lazy<>(() -> U.FACTORY);
      final Lazy<Factory<U>> f_u1 = new Lazy<>(() -> U.FACTORY);
      final Lazy<Factory<E>> f_e = new Lazy<>(() -> E.FACTORY);
      final Lazy<Factory<T>> f_t = new Lazy<>(() -> factoryT);
      final Lazy<Factory<B<Short>>> f_bint16 = new Lazy<>(() -> B.factory(Factories.INT16));
      final Lazy<Factory<HashMap<String, Integer>>> f_smap = new Lazy<>(() -> Factories.stringMap(Factories.INT32));
      final Lazy<Factory<JsonElement>> f_json1 = new Lazy<>(() -> JsonBindings.JSON_FACTORY);
      final Lazy<Factory<JsonElement>> f_json2 = new Lazy<>(() -> JsonBindings.JSON_FACTORY);

      @Override
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
          new A((short)0, "xyz", false),
          U.f_int((short)45),
          U.f_void(),
          E.V2,
          f_t.get().create(),
          new B<Short>((short)56, "yikes", Factories.arrayList((short)1, (short)2, (short)3), new XY<Short>((short)5, (short)5)),
          Factories.stringMap("a", 45, "b", 47),
          JsonHelpers.jsonFromString("null"),
          JsonHelpers.jsonFromString("[{\"v1\":27,\"v2\":\"abcde\"},true]")
          );
      }

      @Override
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
          f_bytes.get().create(other.getF_bytes()),
          other.getF_string(),
          f_vstring.get().create(other.getF_vstring()),
          f_a.get().create(other.getF_a()),
          f_u.get().create(other.getF_u()),
          f_u1.get().create(other.getF_u1()),
          f_e.get().create(other.getF_e()),
          f_t.get().create(other.getF_t()),
          f_bint16.get().create(other.getF_bint16()),
          f_smap.get().create(other.getF_smap()),
          other.getF_json1(),
          other.getF_json2()
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test3", "S");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<S<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<Void>> f_void = new Lazy<>(() -> JsonBindings.VOID);
    final Lazy<JsonBinding<Boolean>> f_bool = new Lazy<>(() -> JsonBindings.BOOLEAN);
    final Lazy<JsonBinding<Byte>> f_int8 = new Lazy<>(() -> JsonBindings.INT8);
    final Lazy<JsonBinding<Short>> f_int16 = new Lazy<>(() -> JsonBindings.INT16);
    final Lazy<JsonBinding<Integer>> f_int32 = new Lazy<>(() -> JsonBindings.INT32);
    final Lazy<JsonBinding<Long>> f_int64 = new Lazy<>(() -> JsonBindings.INT64);
    final Lazy<JsonBinding<Byte>> f_word8 = new Lazy<>(() -> JsonBindings.WORD8);
    final Lazy<JsonBinding<Short>> f_word16 = new Lazy<>(() -> JsonBindings.WORD16);
    final Lazy<JsonBinding<Integer>> f_word32 = new Lazy<>(() -> JsonBindings.WORD32);
    final Lazy<JsonBinding<Long>> f_word64 = new Lazy<>(() -> JsonBindings.WORD64);
    final Lazy<JsonBinding<Float>> f_float = new Lazy<>(() -> JsonBindings.FLOAT);
    final Lazy<JsonBinding<Double>> f_double = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<ByteArray>> f_bytes = new Lazy<>(() -> JsonBindings.BYTE_ARRAY);
    final Lazy<JsonBinding<String>> f_string = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<ArrayList<String>>> f_vstring = new Lazy<>(() -> JsonBindings.arrayList(JsonBindings.STRING));
    final Lazy<JsonBinding<A>> f_a = new Lazy<>(() -> A.jsonBinding());
    final Lazy<JsonBinding<U>> f_u = new Lazy<>(() -> U.jsonBinding());
    final Lazy<JsonBinding<U>> f_u1 = new Lazy<>(() -> U.jsonBinding());
    final Lazy<JsonBinding<E>> f_e = new Lazy<>(() -> E.jsonBinding());
    final Lazy<JsonBinding<T>> f_t = new Lazy<>(() -> bindingT);
    final Lazy<JsonBinding<B<Short>>> f_bint16 = new Lazy<>(() -> B.jsonBinding(JsonBindings.INT16));
    final Lazy<JsonBinding<HashMap<String, Integer>>> f_smap = new Lazy<>(() -> JsonBindings.stringMap(JsonBindings.INT32));
    final Lazy<JsonBinding<JsonElement>> f_json1 = new Lazy<>(() -> JsonBindings.JSON);
    final Lazy<JsonBinding<JsonElement>> f_json2 = new Lazy<>(() -> JsonBindings.JSON);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<S<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<S<T>>() {
      @Override
      public Factory<S<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("f_void", f_void.get().toJson(_value.f_void));
        _result.add("f_bool", f_bool.get().toJson(_value.f_bool));
        _result.add("f_int8", f_int8.get().toJson(_value.f_int8));
        _result.add("f_int16", f_int16.get().toJson(_value.f_int16));
        _result.add("f_int32", f_int32.get().toJson(_value.f_int32));
        _result.add("f_int64", f_int64.get().toJson(_value.f_int64));
        _result.add("f_word8", f_word8.get().toJson(_value.f_word8));
        _result.add("f_word16", f_word16.get().toJson(_value.f_word16));
        _result.add("f_word32", f_word32.get().toJson(_value.f_word32));
        _result.add("f_word64", f_word64.get().toJson(_value.f_word64));
        _result.add("f_float", f_float.get().toJson(_value.f_float));
        _result.add("f_double", f_double.get().toJson(_value.f_double));
        _result.add("f_bytes", f_bytes.get().toJson(_value.f_bytes));
        _result.add("f_string", f_string.get().toJson(_value.f_string));
        _result.add("f_vstring", f_vstring.get().toJson(_value.f_vstring));
        _result.add("f_a", f_a.get().toJson(_value.f_a));
        _result.add("f_u", f_u.get().toJson(_value.f_u));
        _result.add("f_u1", f_u1.get().toJson(_value.f_u1));
        _result.add("f_e", f_e.get().toJson(_value.f_e));
        _result.add("f_t", f_t.get().toJson(_value.f_t));
        _result.add("f_bint16", f_bint16.get().toJson(_value.f_bint16));
        _result.add("f_smap", f_smap.get().toJson(_value.f_smap));
        _result.add("f_json1", f_json1.get().toJson(_value.f_json1));
        _result.add("f_json2", f_json2.get().toJson(_value.f_json2));
        return _result;
      }

      @Override
      public S<T> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S<T>(
          _obj.has("f_void") ? JsonBindings.fieldFromJson(_obj, "f_void", f_void.get()) : null,
          _obj.has("f_bool") ? JsonBindings.fieldFromJson(_obj, "f_bool", f_bool.get()) : true,
          _obj.has("f_int8") ? JsonBindings.fieldFromJson(_obj, "f_int8", f_int8.get()) : (byte)-5,
          _obj.has("f_int16") ? JsonBindings.fieldFromJson(_obj, "f_int16", f_int16.get()) : (short)-10000,
          _obj.has("f_int32") ? JsonBindings.fieldFromJson(_obj, "f_int32", f_int32.get()) : 56,
          _obj.has("f_int64") ? JsonBindings.fieldFromJson(_obj, "f_int64", f_int64.get()) : 40000L,
          _obj.has("f_word8") ? JsonBindings.fieldFromJson(_obj, "f_word8", f_word8.get()) : (byte)32,
          _obj.has("f_word16") ? JsonBindings.fieldFromJson(_obj, "f_word16", f_word16.get()) : (short)50000,
          _obj.has("f_word32") ? JsonBindings.fieldFromJson(_obj, "f_word32", f_word32.get()) : 124456,
          _obj.has("f_word64") ? JsonBindings.fieldFromJson(_obj, "f_word64", f_word64.get()) : 2344L,
          _obj.has("f_float") ? JsonBindings.fieldFromJson(_obj, "f_float", f_float.get()) : 0.5F,
          _obj.has("f_double") ? JsonBindings.fieldFromJson(_obj, "f_double", f_double.get()) : 0.45,
          _obj.has("f_bytes") ? JsonBindings.fieldFromJson(_obj, "f_bytes", f_bytes.get()) : new ByteArray("hello".getBytes()),
          _obj.has("f_string") ? JsonBindings.fieldFromJson(_obj, "f_string", f_string.get()) : "abcd",
          _obj.has("f_vstring") ? JsonBindings.fieldFromJson(_obj, "f_vstring", f_vstring.get()) : Factories.arrayList("xy", "ab"),
          _obj.has("f_a") ? JsonBindings.fieldFromJson(_obj, "f_a", f_a.get()) : new A((short)0, "xyz", false),
          _obj.has("f_u") ? JsonBindings.fieldFromJson(_obj, "f_u", f_u.get()) : U.f_int((short)45),
          _obj.has("f_u1") ? JsonBindings.fieldFromJson(_obj, "f_u1", f_u1.get()) : U.f_void(),
          _obj.has("f_e") ? JsonBindings.fieldFromJson(_obj, "f_e", f_e.get()) : E.V2,
          JsonBindings.fieldFromJson(_obj, "f_t", f_t.get()),
          _obj.has("f_bint16") ? JsonBindings.fieldFromJson(_obj, "f_bint16", f_bint16.get()) : new B<Short>((short)56, "yikes", Factories.arrayList((short)1, (short)2, (short)3), new XY<Short>((short)5, (short)5)),
          _obj.has("f_smap") ? JsonBindings.fieldFromJson(_obj, "f_smap", f_smap.get()) : Factories.stringMap("a", 45, "b", 47),
          _obj.has("f_json1") ? JsonBindings.fieldFromJson(_obj, "f_json1", f_json1.get()) : JsonHelpers.jsonFromString("null"),
          _obj.has("f_json2") ? JsonBindings.fieldFromJson(_obj, "f_json2", f_json2.get()) : JsonHelpers.jsonFromString("[{\"v1\":27,\"v2\":\"abcde\"},true]")
        );
      }
    };
  }
}
