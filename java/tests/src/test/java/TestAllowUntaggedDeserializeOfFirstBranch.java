
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.FileReader;
import java.nio.file.Paths;
import java.nio.file.Path;

import org.adl.runtime.sys.adlast.Module;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonHelpers;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import adl.test30.Lifted;

public class TestAllowUntaggedDeserializeOfFirstBranch {

  @Test
  public void testLiftedDecode01() throws Exception {
    JsonElement jv1 = gson.fromJson("{\"a\":\"abc\", \"b\":42}", JsonElement.class);
    Lifted lifted = Lifted.jsonBinding().fromJson(jv1);
    assertEquals( Lifted.Disc.ORG_FIELD, lifted.getDisc() );
  }

  static Gson gson = new GsonBuilder()
    .serializeNulls()
    .create();
};
