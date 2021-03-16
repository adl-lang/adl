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
import java.util.Optional;

public class S10 {

  /* Members */

  private U10 f1;
  private Optional<U10> f2;
  private U10 f3;
  private Optional<U10> f4;

  /* Constructors */

  public S10(U10 f1, Optional<U10> f2, U10 f3, Optional<U10> f4) {
    this.f1 = Objects.requireNonNull(f1);
    this.f2 = Objects.requireNonNull(f2);
    this.f3 = Objects.requireNonNull(f3);
    this.f4 = Objects.requireNonNull(f4);
  }

  public S10() {
    this.f1 = defF1();
    this.f2 = defF2();
    this.f3 = defF3();
    this.f4 = defF4();
  }

  public S10(S10 other) {
    this.f1 = U10.FACTORY.create(other.f1);
    this.f2 = Factories.nullable(U10.FACTORY).create(other.f2);
    this.f3 = U10.FACTORY.create(other.f3);
    this.f4 = Factories.nullable(U10.FACTORY).create(other.f4);
  }

  /* Field defaults */

  public static U10 defF1() {
    return U10.v2();
  }

  public static Optional<U10> defF2() {
    return Optional.<U10>of(U10.v2());
  }

  public static U10 defF3() {
    return U10.v1((short)17);
  }

  public static Optional<U10> defF4() {
    return Optional.<U10>of(U10.v1((short)17));
  }

  /* Accessors and mutators */

  public U10 getF1() {
    return f1;
  }

  public S10 setF1(U10 f1) {
    this.f1 = Objects.requireNonNull(f1);
    return this;
  }

  public Optional<U10> getF2() {
    return f2;
  }

  public S10 setF2(Optional<U10> f2) {
    this.f2 = Objects.requireNonNull(f2);
    return this;
  }

  public U10 getF3() {
    return f3;
  }

  public S10 setF3(U10 f3) {
    this.f3 = Objects.requireNonNull(f3);
    return this;
  }

  public Optional<U10> getF4() {
    return f4;
  }

  public S10 setF4(Optional<U10> f4) {
    this.f4 = Objects.requireNonNull(f4);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S10)) {
      return false;
    }
    S10 other = (S10) other0;
    return
      f1.equals(other.f1) &&
      f2.equals(other.f2) &&
      f3.equals(other.f3) &&
      f4.equals(other.f4);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + f1.hashCode();
    _result = _result * 37 + f2.hashCode();
    _result = _result * 37 + f3.hashCode();
    _result = _result * 37 + f4.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private U10 f1;
    private Optional<U10> f2;
    private U10 f3;
    private Optional<U10> f4;

    public Builder() {
      this.f1 = U10.v2();
      this.f2 = Optional.<U10>of(U10.v2());
      this.f3 = U10.v1((short)17);
      this.f4 = Optional.<U10>of(U10.v1((short)17));
    }

    public Builder setF1(U10 f1) {
      this.f1 = Objects.requireNonNull(f1);
      return this;
    }

    public Builder setF2(Optional<U10> f2) {
      this.f2 = Objects.requireNonNull(f2);
      return this;
    }

    public Builder setF3(U10 f3) {
      this.f3 = Objects.requireNonNull(f3);
      return this;
    }

    public Builder setF4(Optional<U10> f4) {
      this.f4 = Objects.requireNonNull(f4);
      return this;
    }

    public S10 create() {
      return new S10(f1, f2, f3, f4);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<S10> FACTORY = new Factory<S10>() {
    @Override
    public S10 create(S10 other) {
      return new S10(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "S10");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<S10> jsonBinding() {
      return S10.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<S10> jsonBinding() {
    final Lazy<JsonBinding<U10>> f1 = new Lazy<>(() -> U10.jsonBinding());
    final Lazy<JsonBinding<Optional<U10>>> f2 = new Lazy<>(() -> JsonBindings.nullable(U10.jsonBinding()));
    final Lazy<JsonBinding<U10>> f3 = new Lazy<>(() -> U10.jsonBinding());
    final Lazy<JsonBinding<Optional<U10>>> f4 = new Lazy<>(() -> JsonBindings.nullable(U10.jsonBinding()));
    final Factory<S10> _factory = FACTORY;

    return new JsonBinding<S10>() {
      @Override
      public Factory<S10> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S10 _value) {
        JsonObject _result = new JsonObject();
        _result.add("f1", f1.get().toJson(_value.f1));
        _result.add("f2", f2.get().toJson(_value.f2));
        _result.add("f3", f3.get().toJson(_value.f3));
        _result.add("f4", f4.get().toJson(_value.f4));
        return _result;
      }

      @Override
      public S10 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S10(
          _obj.has("f1") ? JsonBindings.fieldFromJson(_obj, "f1", f1.get()) : U10.v2(),
          _obj.has("f2") ? JsonBindings.fieldFromJson(_obj, "f2", f2.get()) : Optional.<U10>of(U10.v2()),
          _obj.has("f3") ? JsonBindings.fieldFromJson(_obj, "f3", f3.get()) : U10.v1((short)17),
          _obj.has("f4") ? JsonBindings.fieldFromJson(_obj, "f4", f4.get()) : Optional.<U10>of(U10.v1((short)17))
        );
      }
    };
  }
}
