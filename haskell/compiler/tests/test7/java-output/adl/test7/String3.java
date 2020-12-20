/* @generated from adl module test7 */

package adl.test7;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class String3 {

  /* Members */

  private String value;

  /* Constructors */

  public String3(String value) {
    this.value = Objects.requireNonNull(value);
  }

  public String3() {
    this.value = defValue();
  }

  public String3(String3 other) {
    this.value = other.value;
  }

  /* Field defaults */

  public static String defValue() {
    return "hello";
  }

  /* Accessors and mutators */

  public String getValue() {
    return value;
  }

  public String3 setValue(String value) {
    this.value = Objects.requireNonNull(value);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof String3)) {
      return false;
    }
    String3 other = (String3) other0;
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

  public static final Factory<String3> FACTORY = new Factory<String3>() {
    @Override
    public String3 create(String3 other) {
      return new String3(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test7", "String3");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<String3> jsonBinding() {
      return String3.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<String3> jsonBinding() {
    final JsonBinding<String> _binding = JsonBindings.STRING;
    final Factory<String3> _factory = FACTORY;

    return new JsonBinding<String3>() {
      @Override
      public Factory<String3> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(String3 _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public String3 fromJson(JsonElement _json) {
        return new String3(_binding.fromJson(_json));
      }
    };
  }
}
