/* @generated from adl module admin */

package adl.admin;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.TypeToken;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class Holder {

  /* Members */

  private AdminApiRequests admin;

  /* Constructors */

  public Holder(AdminApiRequests admin) {
    this.admin = Objects.requireNonNull(admin);
  }

  public Holder() {
    this.admin = new AdminApiRequests(new HttpGet<JsonElement>("/admin/query", new TypeToken<JsonElement>(JsonBindings.JSON)));
  }

  public Holder(Holder other) {
    this.admin = AdminApiRequests.FACTORY.create(other.admin);
  }

  /* Accessors and mutators */

  public AdminApiRequests getAdmin() {
    return admin;
  }

  public void setAdmin(AdminApiRequests admin) {
    this.admin = Objects.requireNonNull(admin);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Holder)) {
      return false;
    }
    Holder other = (Holder) other0;
    return
      admin.equals(other.admin);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + admin.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Holder> FACTORY = new Factory<Holder>() {
    @Override
    public Holder create(Holder other) {
      return new Holder(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("admin", "Holder");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Holder> jsonBinding() {
      return Holder.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Holder> jsonBinding() {
    final Lazy<JsonBinding<AdminApiRequests>> admin = new Lazy<>(() -> AdminApiRequests.jsonBinding());
    final Factory<Holder> _factory = FACTORY;

    return new JsonBinding<Holder>() {
      @Override
      public Factory<Holder> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Holder _value) {
        JsonObject _result = new JsonObject();
        _result.add("admin", admin.get().toJson(_value.admin));
        return _result;
      }

      @Override
      public Holder fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Holder(
          _obj.has("admin") ? JsonBindings.fieldFromJson(_obj, "admin", admin.get()) : new AdminApiRequests(new HttpGet<JsonElement>("/admin/query", new TypeToken<JsonElement>(JsonBindings.JSON)))
        );
      }
    };
  }
}
