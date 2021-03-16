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

public class S11 {

  /* Members */

  private U11 f1;
  private Optional<U11> f2;
  private U11 f3;
  private Optional<U11> f4;

  /* Constructors */

  public S11(U11 f1, Optional<U11> f2, U11 f3, Optional<U11> f4) {
    this.f1 = Objects.requireNonNull(f1);
    this.f2 = Objects.requireNonNull(f2);
    this.f3 = Objects.requireNonNull(f3);
    this.f4 = Objects.requireNonNull(f4);
  }

  public S11() {
    this.f1 = defF1();
    this.f2 = defF2();
    this.f3 = defF3();
    this.f4 = defF4();
  }

  public S11(S11 other) {
    this.f1 = U11.FACTORY.create(other.f1);
    this.f2 = Factories.nullable(U11.FACTORY).create(other.f2);
    this.f3 = U11.FACTORY.create(other.f3);
    this.f4 = Factories.nullable(U11.FACTORY).create(other.f4);
  }

  /* Field defaults */

  public static U11 defF1() {
    return U11.v2();
  }

  public static Optional<U11> defF2() {
    return Optional.<U11>of(U11.v2());
  }

  public static U11 defF3() {
    return U11.v1((short)17);
  }

  public static Optional<U11> defF4() {
    return Optional.<U11>of(U11.v1((short)17));
  }

  /* Accessors and mutators */

  public U11 getF1() {
    return f1;
  }

  public S11 setF1(U11 f1) {
    this.f1 = Objects.requireNonNull(f1);
    return this;
  }

  public Optional<U11> getF2() {
    return f2;
  }

  public S11 setF2(Optional<U11> f2) {
    this.f2 = Objects.requireNonNull(f2);
    return this;
  }

  public U11 getF3() {
    return f3;
  }

  public S11 setF3(U11 f3) {
    this.f3 = Objects.requireNonNull(f3);
    return this;
  }

  public Optional<U11> getF4() {
    return f4;
  }

  public S11 setF4(Optional<U11> f4) {
    this.f4 = Objects.requireNonNull(f4);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S11)) {
      return false;
    }
    S11 other = (S11) other0;
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
    private U11 f1;
    private Optional<U11> f2;
    private U11 f3;
    private Optional<U11> f4;

    public Builder() {
      this.f1 = U11.v2();
      this.f2 = Optional.<U11>of(U11.v2());
      this.f3 = U11.v1((short)17);
      this.f4 = Optional.<U11>of(U11.v1((short)17));
    }

    public Builder setF1(U11 f1) {
      this.f1 = Objects.requireNonNull(f1);
      return this;
    }

    public Builder setF2(Optional<U11> f2) {
      this.f2 = Objects.requireNonNull(f2);
      return this;
    }

    public Builder setF3(U11 f3) {
      this.f3 = Objects.requireNonNull(f3);
      return this;
    }

    public Builder setF4(Optional<U11> f4) {
      this.f4 = Objects.requireNonNull(f4);
      return this;
    }

    public S11 create() {
      return new S11(f1, f2, f3, f4);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<S11> FACTORY = new Factory<S11>() {
    @Override
    public S11 create(S11 other) {
      return new S11(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "S11");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<S11> jsonBinding() {
      return S11.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<S11> jsonBinding() {
    final Lazy<JsonBinding<U11>> f1 = new Lazy<>(() -> U11.jsonBinding());
    final Lazy<JsonBinding<Optional<U11>>> f2 = new Lazy<>(() -> JsonBindings.nullable(U11.jsonBinding()));
    final Lazy<JsonBinding<U11>> f3 = new Lazy<>(() -> U11.jsonBinding());
    final Lazy<JsonBinding<Optional<U11>>> f4 = new Lazy<>(() -> JsonBindings.nullable(U11.jsonBinding()));
    final Factory<S11> _factory = FACTORY;

    return new JsonBinding<S11>() {
      @Override
      public Factory<S11> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S11 _value) {
        JsonObject _result = new JsonObject();
        _result.add("f1", f1.get().toJson(_value.f1));
        _result.add("f2", f2.get().toJson(_value.f2));
        _result.add("f3", f3.get().toJson(_value.f3));
        _result.add("f4", f4.get().toJson(_value.f4));
        return _result;
      }

      @Override
      public S11 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S11(
          _obj.has("f1") ? JsonBindings.fieldFromJson(_obj, "f1", f1.get()) : U11.v2(),
          _obj.has("f2") ? JsonBindings.fieldFromJson(_obj, "f2", f2.get()) : Optional.<U11>of(U11.v2()),
          _obj.has("f3") ? JsonBindings.fieldFromJson(_obj, "f3", f3.get()) : U11.v1((short)17),
          _obj.has("f4") ? JsonBindings.fieldFromJson(_obj, "f4", f4.get()) : Optional.<U11>of(U11.v1((short)17))
        );
      }
    };
  }
}
