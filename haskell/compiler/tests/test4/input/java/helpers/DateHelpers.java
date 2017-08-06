package helpers;

import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.sys.adlast.TypeExpr;
import org.adl.sys.adlast.TypeRef;
import org.adl.sys.adlast.ScopedName;
import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;

import java.util.ArrayList;
import java.time.LocalDate;

/**
 * @author timd
 */
public class DateHelpers {

  public static LocalDate create(String value) {
    return LocalDate.parse(value);
  }

  public static final Factory<LocalDate> FACTORY = new Factory<LocalDate>() {
    @Override
    public LocalDate create() {
      return LocalDate.parse("1970-01-01");
    }

    @Override
    public LocalDate create(LocalDate other) {
      return other;  // immutable type
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test4", "Date");
      return new TypeExpr(TypeRef.reference(scopedName), new ArrayList<>());
    }
  };

  /* Json serialization */

  public static JsonBinding<LocalDate> jsonBinding() {
    return new JsonBinding<LocalDate>() {
      @Override
      public Factory<LocalDate> factory() {
        return FACTORY;
      }

      @Override
      public JsonElement toJson(LocalDate value) {
        return new JsonPrimitive(value.toString());
      }

      @Override
      public LocalDate fromJson(JsonElement json) {
        return LocalDate.parse(json.getAsString());
      }
    };
  }
}
