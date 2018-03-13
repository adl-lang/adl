/* Code generated from adl module test17 */

package adl.test17;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class X1 {

  /* Members */

  private int f1;
  private int f2;
  private Pair<String, Integer> f3;
  private Pair<String, String> f4;
  private ArrayList<Integer> f5;
  private ArrayList<Pair<String, Integer>> f6;
  private ArrayList<Pair<String, String>> f7;

  /* Constructors */

  public X1(int f1, int f2, Pair<String, Integer> f3, Pair<String, String> f4, ArrayList<Integer> f5, ArrayList<Pair<String, Integer>> f6, ArrayList<Pair<String, String>> f7) {
    this.f1 = f1;
    this.f2 = f2;
    this.f3 = Objects.requireNonNull(f3);
    this.f4 = Objects.requireNonNull(f4);
    this.f5 = Objects.requireNonNull(f5);
    this.f6 = Objects.requireNonNull(f6);
    this.f7 = Objects.requireNonNull(f7);
  }

  public X1() {
    this.f1 = 0;
    this.f2 = 0;
    this.f3 = Pair.factory(Factories.STRING, Factories.INT32).create();
    this.f4 = Pair.factory(Factories.STRING, Factories.STRING).create();
    this.f5 = new ArrayList<Integer>();
    this.f6 = new ArrayList<Pair<String, Integer>>();
    this.f7 = new ArrayList<Pair<String, String>>();
  }

  public X1(X1 other) {
    this.f1 = other.f1;
    this.f2 = other.f2;
    this.f3 = Pair.factory(Factories.STRING, Factories.INT32).create(other.f3);
    this.f4 = Pair.factory(Factories.STRING, Factories.STRING).create(other.f4);
    this.f5 = Factories.arrayList(Factories.INT32).create(other.f5);
    this.f6 = Factories.arrayList(Pair.factory(Factories.STRING, Factories.INT32)).create(other.f6);
    this.f7 = Factories.arrayList(Pair.factory(Factories.STRING, Factories.STRING)).create(other.f7);
  }

  /* Accessors and mutators */

  public int getF1() {
    return f1;
  }

  public void setF1(int f1) {
    this.f1 = f1;
  }

  public int getF2() {
    return f2;
  }

  public void setF2(int f2) {
    this.f2 = f2;
  }

  public Pair<String, Integer> getF3() {
    return f3;
  }

  public void setF3(Pair<String, Integer> f3) {
    this.f3 = Objects.requireNonNull(f3);
  }

  public Pair<String, String> getF4() {
    return f4;
  }

  public void setF4(Pair<String, String> f4) {
    this.f4 = Objects.requireNonNull(f4);
  }

  public ArrayList<Integer> getF5() {
    return f5;
  }

  public void setF5(ArrayList<Integer> f5) {
    this.f5 = Objects.requireNonNull(f5);
  }

  public ArrayList<Pair<String, Integer>> getF6() {
    return f6;
  }

  public void setF6(ArrayList<Pair<String, Integer>> f6) {
    this.f6 = Objects.requireNonNull(f6);
  }

  public ArrayList<Pair<String, String>> getF7() {
    return f7;
  }

  public void setF7(ArrayList<Pair<String, String>> f7) {
    this.f7 = Objects.requireNonNull(f7);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof X1)) {
      return false;
    }
    X1 other = (X1) other0;
    return
      f1 == other.f1 &&
      f2 == other.f2 &&
      f3.equals(other.f3) &&
      f4.equals(other.f4) &&
      f5.equals(other.f5) &&
      f6.equals(other.f6) &&
      f7.equals(other.f7);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + f1;
    _result = _result * 37 + f2;
    _result = _result * 37 + f3.hashCode();
    _result = _result * 37 + f4.hashCode();
    _result = _result * 37 + f5.hashCode();
    _result = _result * 37 + f6.hashCode();
    _result = _result * 37 + f7.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private Integer f1;
    private Integer f2;
    private Pair<String, Integer> f3;
    private Pair<String, String> f4;
    private ArrayList<Integer> f5;
    private ArrayList<Pair<String, Integer>> f6;
    private ArrayList<Pair<String, String>> f7;

    public Builder() {
      this.f1 = null;
      this.f2 = null;
      this.f3 = null;
      this.f4 = null;
      this.f5 = null;
      this.f6 = null;
      this.f7 = null;
    }

    public Builder setF1(Integer f1) {
      this.f1 = Objects.requireNonNull(f1);
      return this;
    }

    public Builder setF2(Integer f2) {
      this.f2 = Objects.requireNonNull(f2);
      return this;
    }

    public Builder setF3(Pair<String, Integer> f3) {
      this.f3 = Objects.requireNonNull(f3);
      return this;
    }

    public Builder setF4(Pair<String, String> f4) {
      this.f4 = Objects.requireNonNull(f4);
      return this;
    }

    public Builder setF5(ArrayList<Integer> f5) {
      this.f5 = Objects.requireNonNull(f5);
      return this;
    }

    public Builder setF6(ArrayList<Pair<String, Integer>> f6) {
      this.f6 = Objects.requireNonNull(f6);
      return this;
    }

    public Builder setF7(ArrayList<Pair<String, String>> f7) {
      this.f7 = Objects.requireNonNull(f7);
      return this;
    }

    public X1 create() {
      Builders.checkFieldInitialized("X1", "f1", f1);
      Builders.checkFieldInitialized("X1", "f2", f2);
      Builders.checkFieldInitialized("X1", "f3", f3);
      Builders.checkFieldInitialized("X1", "f4", f4);
      Builders.checkFieldInitialized("X1", "f5", f5);
      Builders.checkFieldInitialized("X1", "f6", f6);
      Builders.checkFieldInitialized("X1", "f7", f7);
      return new X1(f1, f2, f3, f4, f5, f6, f7);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<X1> FACTORY = new Factory<X1>() {
    @Override
    public X1 create() {
      return new X1();
    }

    @Override
    public X1 create(X1 other) {
      return new X1(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test17", "X1");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<X1> jsonBinding() {
    final Lazy<JsonBinding<Integer>> f1 = new Lazy<>(() -> JsonBindings.INT32);
    final Lazy<JsonBinding<Integer>> f2 = new Lazy<>(() -> JsonBindings.INT32);
    final Lazy<JsonBinding<Pair<String, Integer>>> f3 = new Lazy<>(() -> Pair.jsonBinding(JsonBindings.STRING, JsonBindings.INT32));
    final Lazy<JsonBinding<Pair<String, String>>> f4 = new Lazy<>(() -> Pair.jsonBinding(JsonBindings.STRING, JsonBindings.STRING));
    final Lazy<JsonBinding<ArrayList<Integer>>> f5 = new Lazy<>(() -> JsonBindings.arrayList(JsonBindings.INT32));
    final Lazy<JsonBinding<ArrayList<Pair<String, Integer>>>> f6 = new Lazy<>(() -> JsonBindings.arrayList(Pair.jsonBinding(JsonBindings.STRING, JsonBindings.INT32)));
    final Lazy<JsonBinding<ArrayList<Pair<String, String>>>> f7 = new Lazy<>(() -> JsonBindings.arrayList(Pair.jsonBinding(JsonBindings.STRING, JsonBindings.STRING)));
    final Factory<X1> _factory = FACTORY;

    return new JsonBinding<X1>() {
      @Override
      public Factory<X1> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(X1 _value) {
        JsonObject _result = new JsonObject();
        _result.add("f1", f1.get().toJson(_value.f1));
        _result.add("f2", f2.get().toJson(_value.f2));
        _result.add("f3", f3.get().toJson(_value.f3));
        _result.add("f4", f4.get().toJson(_value.f4));
        _result.add("f5", f5.get().toJson(_value.f5));
        _result.add("f6", f6.get().toJson(_value.f6));
        _result.add("f7", f7.get().toJson(_value.f7));
        return _result;
      }

      @Override
      public X1 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new X1(
          JsonBindings.fieldFromJson(_obj, "f1", f1.get()),
          JsonBindings.fieldFromJson(_obj, "f2", f2.get()),
          JsonBindings.fieldFromJson(_obj, "f3", f3.get()),
          JsonBindings.fieldFromJson(_obj, "f4", f4.get()),
          JsonBindings.fieldFromJson(_obj, "f5", f5.get()),
          JsonBindings.fieldFromJson(_obj, "f6", f6.get()),
          JsonBindings.fieldFromJson(_obj, "f7", f7.get())
        );
      }
    };
  }
}
