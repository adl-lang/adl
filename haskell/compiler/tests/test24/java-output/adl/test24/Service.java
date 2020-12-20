/* @generated from adl module test24 */

package adl.test24;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
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
import java.util.Optional;

public class Service {

  /* Members */

  private PostReq<String, String> hello;
  private PostReq<Optional<String>, Optional<String>> farewell;
  private CrudReqs<JsonElement> blobs;

  /* Constructors */

  public Service(PostReq<String, String> hello, PostReq<Optional<String>, Optional<String>> farewell, CrudReqs<JsonElement> blobs) {
    this.hello = Objects.requireNonNull(hello);
    this.farewell = Objects.requireNonNull(farewell);
    this.blobs = Objects.requireNonNull(blobs);
  }

  public Service() {
    this.hello = defHello();
    this.farewell = defFarewell();
    this.blobs = defBlobs();
  }

  public Service(Service other) {
    this.hello = PostReq.factory(Factories.STRING, Factories.STRING).create(other.hello);
    this.farewell = PostReq.factory(Factories.nullable(Factories.STRING), Factories.nullable(Factories.STRING)).create(other.farewell);
    this.blobs = CrudReqs.factory(JsonBindings.JSON_FACTORY).create(other.blobs);
  }

  /* Field defaults */

  public static PostReq<String, String> defHello() {
    return new PostReq<String, String>("/hello", new TypeToken<String>(JsonBindings.STRING), new TypeToken<String>(JsonBindings.STRING));
  }

  public static PostReq<Optional<String>, Optional<String>> defFarewell() {
    return new PostReq<Optional<String>, Optional<String>>("/farewell", new TypeToken<Optional<String>>(JsonBindings.nullable(JsonBindings.STRING)), new TypeToken<Optional<String>>(JsonBindings.nullable(JsonBindings.STRING)));
  }

  public static CrudReqs<JsonElement> defBlobs() {
    return new CrudReqs<JsonElement>(new PostReq<JsonElement, String>("/blobs/create", new TypeToken<JsonElement>(JsonBindings.JSON), new TypeToken<String>(JsonBindings.STRING)), new PostReq<String, JsonElement>("/blobs/read", new TypeToken<String>(JsonBindings.STRING), new TypeToken<JsonElement>(JsonBindings.JSON)));
  }

  /* Accessors and mutators */

  public PostReq<String, String> getHello() {
    return hello;
  }

  public Service setHello(PostReq<String, String> hello) {
    this.hello = Objects.requireNonNull(hello);
    return this;
  }

  public PostReq<Optional<String>, Optional<String>> getFarewell() {
    return farewell;
  }

  public Service setFarewell(PostReq<Optional<String>, Optional<String>> farewell) {
    this.farewell = Objects.requireNonNull(farewell);
    return this;
  }

  public CrudReqs<JsonElement> getBlobs() {
    return blobs;
  }

  public Service setBlobs(CrudReqs<JsonElement> blobs) {
    this.blobs = Objects.requireNonNull(blobs);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Service)) {
      return false;
    }
    Service other = (Service) other0;
    return
      hello.equals(other.hello) &&
      farewell.equals(other.farewell) &&
      blobs.equals(other.blobs);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + hello.hashCode();
    _result = _result * 37 + farewell.hashCode();
    _result = _result * 37 + blobs.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private PostReq<String, String> hello;
    private PostReq<Optional<String>, Optional<String>> farewell;
    private CrudReqs<JsonElement> blobs;

    public Builder() {
      this.hello = new PostReq<String, String>("/hello", new TypeToken<String>(JsonBindings.STRING), new TypeToken<String>(JsonBindings.STRING));
      this.farewell = new PostReq<Optional<String>, Optional<String>>("/farewell", new TypeToken<Optional<String>>(JsonBindings.nullable(JsonBindings.STRING)), new TypeToken<Optional<String>>(JsonBindings.nullable(JsonBindings.STRING)));
      this.blobs = new CrudReqs<JsonElement>(new PostReq<JsonElement, String>("/blobs/create", new TypeToken<JsonElement>(JsonBindings.JSON), new TypeToken<String>(JsonBindings.STRING)), new PostReq<String, JsonElement>("/blobs/read", new TypeToken<String>(JsonBindings.STRING), new TypeToken<JsonElement>(JsonBindings.JSON)));
    }

    public Builder setHello(PostReq<String, String> hello) {
      this.hello = Objects.requireNonNull(hello);
      return this;
    }

    public Builder setFarewell(PostReq<Optional<String>, Optional<String>> farewell) {
      this.farewell = Objects.requireNonNull(farewell);
      return this;
    }

    public Builder setBlobs(CrudReqs<JsonElement> blobs) {
      this.blobs = Objects.requireNonNull(blobs);
      return this;
    }

    public Service create() {
      return new Service(hello, farewell, blobs);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<Service> FACTORY = new Factory<Service>() {
    @Override
    public Service create(Service other) {
      return new Service(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test24", "Service");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Service> jsonBinding() {
      return Service.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Service> jsonBinding() {
    final Lazy<JsonBinding<PostReq<String, String>>> hello = new Lazy<>(() -> PostReq.jsonBinding(JsonBindings.STRING, JsonBindings.STRING));
    final Lazy<JsonBinding<PostReq<Optional<String>, Optional<String>>>> farewell = new Lazy<>(() -> PostReq.jsonBinding(JsonBindings.nullable(JsonBindings.STRING), JsonBindings.nullable(JsonBindings.STRING)));
    final Lazy<JsonBinding<CrudReqs<JsonElement>>> blobs = new Lazy<>(() -> CrudReqs.jsonBinding(JsonBindings.JSON));
    final Factory<Service> _factory = FACTORY;

    return new JsonBinding<Service>() {
      @Override
      public Factory<Service> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Service _value) {
        JsonObject _result = new JsonObject();
        _result.add("hello", hello.get().toJson(_value.hello));
        _result.add("farewell", farewell.get().toJson(_value.farewell));
        _result.add("blobs", blobs.get().toJson(_value.blobs));
        return _result;
      }

      @Override
      public Service fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Service(
          _obj.has("hello") ? JsonBindings.fieldFromJson(_obj, "hello", hello.get()) : new PostReq<String, String>("/hello", new TypeToken<String>(JsonBindings.STRING), new TypeToken<String>(JsonBindings.STRING)),
          _obj.has("farewell") ? JsonBindings.fieldFromJson(_obj, "farewell", farewell.get()) : new PostReq<Optional<String>, Optional<String>>("/farewell", new TypeToken<Optional<String>>(JsonBindings.nullable(JsonBindings.STRING)), new TypeToken<Optional<String>>(JsonBindings.nullable(JsonBindings.STRING))),
          _obj.has("blobs") ? JsonBindings.fieldFromJson(_obj, "blobs", blobs.get()) : new CrudReqs<JsonElement>(new PostReq<JsonElement, String>("/blobs/create", new TypeToken<JsonElement>(JsonBindings.JSON), new TypeToken<String>(JsonBindings.STRING)), new PostReq<String, JsonElement>("/blobs/read", new TypeToken<String>(JsonBindings.STRING), new TypeToken<JsonElement>(JsonBindings.JSON)))
        );
      }
    };
  }
}
