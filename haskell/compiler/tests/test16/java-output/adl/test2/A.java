/* Code generated from adl module test2 */

package adl.test2;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.sys.adlast.ScopedName;
import org.adl.sys.adlast.TypeExpr;
import org.adl.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class A {

  /* Members */

  private int a;

  /* Constructors */

  public A(int a) {
    this.a = a;
  }

  public A() {
    this.a = 0;
  }

  public A(A other) {
    this.a = other.a;
  }

  /* Accessors and mutators */

  public int getA() {
    return a;
  }

  public void setA(int a) {
    this.a = a;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof A)) {
      return false;
    }
    A other = (A) other0;
    return
      a == other.a;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + a;
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<A> FACTORY = new Factory<A>() {
    @Override
    public A create() {
      return new A();
    }

    @Override
    public A create(A other) {
      return new A(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test2", "A");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<A> jsonBinding() {
    final Lazy<JsonBinding<Integer>> a = new Lazy<>(() -> JsonBindings.INT32);
    final Factory<A> _factory = FACTORY;

    return new JsonBinding<A>() {
      @Override
      public Factory<A> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(A _value) {
        JsonObject _result = new JsonObject();
        _result.add("a", a.get().toJson(_value.a));
        return _result;
      }

      @Override
      public A fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new A(
          JsonBindings.fieldFromJson(_obj, "a", a.get())
        );
      }
    };
  }
}
