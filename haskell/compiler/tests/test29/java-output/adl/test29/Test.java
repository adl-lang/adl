/* @generated from adl module test29 */

package adl.test29;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * An example with weird "quoting" conventions, designed to break things
 */
public class Test {

  /* Members */

  private Map<String, String> foo;

  /* Constructors */

  public Test(Map<String, String> foo) {
    this.foo = Objects.requireNonNull(foo);
  }

  public Test() {
    this.foo = Factories.stringMap(" ", "baz", "\"", "baz", "$", "bar", "'", "baz", "degrees", "°");
  }

  public Test(Test other) {
    this.foo = Factories.stringMap(Factories.STRING).create(other.foo);
  }

  /* Accessors and mutators */

  public Map<String, String> getFoo() {
    return foo;
  }

  public void setFoo(Map<String, String> foo) {
    this.foo = Objects.requireNonNull(foo);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Test)) {
      return false;
    }
    Test other = (Test) other0;
    return
      foo.equals(other.foo);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + foo.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Test> FACTORY = new Factory<Test>() {
    @Override
    public Test create() {
      return new Test();
    }

    @Override
    public Test create(Test other) {
      return new Test(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test29", "Test");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Test> jsonBinding() {
      return Test.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Test> jsonBinding() {
    final Lazy<JsonBinding<Map<String, String>>> foo = new Lazy<>(() -> JsonBindings.stringMap(JsonBindings.STRING));
    final Factory<Test> _factory = FACTORY;

    return new JsonBinding<Test>() {
      @Override
      public Factory<Test> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Test _value) {
        JsonObject _result = new JsonObject();
        _result.add("foo", foo.get().toJson(_value.foo));
        return _result;
      }

      @Override
      public Test fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Test(
          _obj.has("foo") ? JsonBindings.fieldFromJson(_obj, "foo", foo.get()) : Factories.stringMap(" ", "baz", "\"", "baz", "$", "bar", "'", "baz", "degrees", "°")
        );
      }
    };
  }
}
