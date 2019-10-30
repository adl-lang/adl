/* @generated from adl module test */

package adl.test;

import adl.test2.A;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class X {

  /* Members */

  private A value;

  /* Constructors */

  public X(A value) {
    this.value = Objects.requireNonNull(value);
  }

  public X() {
    this.value = new A();
  }

  public X(X other) {
    this.value = A.FACTORY.create(other.value);
  }

  /* Accessors and mutators */

  public A getValue() {
    return value;
  }

  public void setValue(A value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof X)) {
      return false;
    }
    X other = (X) other0;
    return
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + value.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<X> FACTORY = new Factory<X>() {
    @Override
    public X create() {
      return new X();
    }

    @Override
    public X create(X other) {
      return new X(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test", "X");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<X> jsonBinding() {
      return X.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<X> jsonBinding() {
    final JsonBinding<A> _binding = A.jsonBinding();
    final Factory<X> _factory = FACTORY;

    return new JsonBinding<X>() {
      @Override
      public Factory<X> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(X _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public X fromJson(JsonElement _json) {
        return new X(_binding.fromJson(_json));
      }
    };
  }
}
