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

public class String2 {

  /* Members */

  private String value;

  /* Constructors */

  public String2(String value) {
    this.value = Objects.requireNonNull(value);
  }

  public String2() {
    this.value = "";
  }

  public String2(String2 other) {
    this.value = other.value;
  }

  /* Accessors and mutators */

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof String2)) {
      return false;
    }
    String2 other = (String2) other0;
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

  public static final Factory<String2> FACTORY = new Factory<String2>() {
    @Override
    public String2 create() {
      return new String2();
    }

    @Override
    public String2 create(String2 other) {
      return new String2(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test7", "String2");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<String2> jsonBinding() {
    final JsonBinding<String> _binding = JsonBindings.STRING;
    final Factory<String2> _factory = FACTORY;

    return new JsonBinding<String2>() {
      @Override
      public Factory<String2> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(String2 _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public String2 fromJson(JsonElement _json) {
        return new String2(_binding.fromJson(_json));
      }
    };
  }
}
