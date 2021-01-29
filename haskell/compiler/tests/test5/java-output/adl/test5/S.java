/* @generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.AdlVoid;
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

public class S {

  /* Members */

  private U9<String> f1;
  private U9<String> f2;
  private U9<String> f3;

  /* Constructors */

  public S(U9<String> f1, U9<String> f2, U9<String> f3) {
    this.f1 = Objects.requireNonNull(f1);
    this.f2 = Objects.requireNonNull(f2);
    this.f3 = Objects.requireNonNull(f3);
  }

  public S() {
    this.f1 = defF1();
    this.f2 = defF2();
    this.f3 = defF3();
  }

  public S(S other) {
    this.f1 = U9.factory(Factories.STRING).create(other.f1);
    this.f2 = U9.factory(Factories.STRING).create(other.f2);
    this.f3 = U9.factory(Factories.STRING).create(other.f3);
  }

  /* Field defaults */

  public static U9<String> defF1() {
    return U9.v1("xx");
  }

  public static U9<String> defF2() {
    return U9.v2((short)100);
  }

  public static U9<String> defF3() {
    return U9.v3();
  }

  /* Accessors and mutators */

  public U9<String> getF1() {
    return f1;
  }

  public S setF1(U9<String> f1) {
    this.f1 = Objects.requireNonNull(f1);
    return this;
  }

  public U9<String> getF2() {
    return f2;
  }

  public S setF2(U9<String> f2) {
    this.f2 = Objects.requireNonNull(f2);
    return this;
  }

  public U9<String> getF3() {
    return f3;
  }

  public S setF3(U9<String> f3) {
    this.f3 = Objects.requireNonNull(f3);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S)) {
      return false;
    }
    S other = (S) other0;
    return
      f1.equals(other.f1) &&
      f2.equals(other.f2) &&
      f3.equals(other.f3);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + f1.hashCode();
    _result = _result * 37 + f2.hashCode();
    _result = _result * 37 + f3.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private U9<String> f1;
    private U9<String> f2;
    private U9<String> f3;

    public Builder() {
      this.f1 = U9.v1("xx");
      this.f2 = U9.v2((short)100);
      this.f3 = U9.v3();
    }

    public Builder setF1(U9<String> f1) {
      this.f1 = Objects.requireNonNull(f1);
      return this;
    }

    public Builder setF2(U9<String> f2) {
      this.f2 = Objects.requireNonNull(f2);
      return this;
    }

    public Builder setF3(U9<String> f3) {
      this.f3 = Objects.requireNonNull(f3);
      return this;
    }

    public S create() {
      return new S(f1, f2, f3);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<S> FACTORY = new Factory<S>() {
    @Override
    public S create(S other) {
      return new S(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "S");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<S> jsonBinding() {
      return S.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<S> jsonBinding() {
    final Lazy<JsonBinding<U9<String>>> f1 = new Lazy<>(() -> U9.jsonBinding(JsonBindings.STRING));
    final Lazy<JsonBinding<U9<String>>> f2 = new Lazy<>(() -> U9.jsonBinding(JsonBindings.STRING));
    final Lazy<JsonBinding<U9<String>>> f3 = new Lazy<>(() -> U9.jsonBinding(JsonBindings.STRING));
    final Factory<S> _factory = FACTORY;

    return new JsonBinding<S>() {
      @Override
      public Factory<S> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S _value) {
        JsonObject _result = new JsonObject();
        _result.add("f1", f1.get().toJson(_value.f1));
        _result.add("f2", f2.get().toJson(_value.f2));
        _result.add("f3", f3.get().toJson(_value.f3));
        return _result;
      }

      @Override
      public S fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S(
          _obj.has("f1") ? JsonBindings.fieldFromJson(_obj, "f1", f1.get()) : U9.v1("xx"),
          _obj.has("f2") ? JsonBindings.fieldFromJson(_obj, "f2", f2.get()) : U9.v2((short)100),
          _obj.has("f3") ? JsonBindings.fieldFromJson(_obj, "f3", f3.get()) : U9.v3()
        );
      }
    };
  }
}
