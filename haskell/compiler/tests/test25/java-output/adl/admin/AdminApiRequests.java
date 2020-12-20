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

public class AdminApiRequests {

  /* Members */

  private HttpGet<JsonElement> query;

  /* Constructors */

  public AdminApiRequests(HttpGet<JsonElement> query) {
    this.query = Objects.requireNonNull(query);
  }

  public AdminApiRequests() {
    this.query = defQuery();
  }

  public AdminApiRequests(AdminApiRequests other) {
    this.query = HttpGet.factory(JsonBindings.JSON_FACTORY).create(other.query);
  }

  /* Field defaults */

  public static HttpGet<JsonElement> defQuery() {
    return new HttpGet<JsonElement>("/admin/query", new TypeToken<JsonElement>(JsonBindings.JSON));
  }

  /* Accessors and mutators */

  public HttpGet<JsonElement> getQuery() {
    return query;
  }

  public AdminApiRequests setQuery(HttpGet<JsonElement> query) {
    this.query = Objects.requireNonNull(query);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof AdminApiRequests)) {
      return false;
    }
    AdminApiRequests other = (AdminApiRequests) other0;
    return
      query.equals(other.query);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + query.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<AdminApiRequests> FACTORY = new Factory<AdminApiRequests>() {
    @Override
    public AdminApiRequests create(AdminApiRequests other) {
      return new AdminApiRequests(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("admin", "AdminApiRequests");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<AdminApiRequests> jsonBinding() {
      return AdminApiRequests.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<AdminApiRequests> jsonBinding() {
    final Lazy<JsonBinding<HttpGet<JsonElement>>> query = new Lazy<>(() -> HttpGet.jsonBinding(JsonBindings.JSON));
    final Factory<AdminApiRequests> _factory = FACTORY;

    return new JsonBinding<AdminApiRequests>() {
      @Override
      public Factory<AdminApiRequests> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(AdminApiRequests _value) {
        JsonObject _result = new JsonObject();
        _result.add("query", query.get().toJson(_value.query));
        return _result;
      }

      @Override
      public AdminApiRequests fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new AdminApiRequests(
          _obj.has("query") ? JsonBindings.fieldFromJson(_obj, "query", query.get()) : new HttpGet<JsonElement>("/admin/query", new TypeToken<JsonElement>(JsonBindings.JSON))
        );
      }
    };
  }
}
