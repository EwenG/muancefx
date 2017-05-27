package muancefx;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import java.io.InputStreamReader;

public class Main {
  public static void main(String[] args) throws Exception {
    ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
    engine.eval(new InputStreamReader(Main.class.getResourceAsStream("/app.min.js")));
    engine.eval("muancefx.core.start_my_app();");
  }
}
