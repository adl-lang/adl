/* @generated from adl module test30 */

package adl.test30;

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
import java.util.Objects;

public class BStruct {

  /* Members */

  private String foo;

  /* Constructors */

  public BStruct(String foo) {
    this.foo = Objects.requireNonNull(foo);
  }

  public BStruct() {
    this.foo = defFoo();
  }

  public BStruct(BStruct other) {
    this.foo = other.foo;
  }

  /* Field defaults */

  public static String defFoo() {
    return "abc $10";
  }

  /* Accessors and mutators */

  public String getFoo() {
    return foo;
  }

  public BStruct setFoo(String foo) {
    this.foo = Objects.requireNonNull(foo);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof BStruct)) {
      return false;
    }
    BStruct other = (BStruct) other0;
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

  public static final Factory<BStruct> FACTORY = new Factory<BStruct>() {
    @Override
    public BStruct create(BStruct other) {
      return new BStruct(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test30", "BStruct");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<BStruct> jsonBinding() {
      return BStruct.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<BStruct> jsonBinding() {
    final Lazy<JsonBinding<String>> foo = new Lazy<>(() -> JsonBindings.STRING);
    final Factory<BStruct> _factory = FACTORY;

    return new JsonBinding<BStruct>() {
      @Override
      public Factory<BStruct> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(BStruct _value) {
        JsonObject _result = new JsonObject();
        _result.add("foo", foo.get().toJson(_value.foo));
        return _result;
      }

      @Override
      public BStruct fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new BStruct(
          _obj.has("foo") ? JsonBindings.fieldFromJson(_obj, "foo", foo.get()) : "abc $10"
        );
      }
    };
  }
}
