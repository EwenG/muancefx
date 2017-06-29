package muancefx;

import jdk.nashorn.api.scripting.ScriptObjectMirror;

public class Callback implements javafx.util.Callback {
  private ScriptObjectMirror factoryFn = null;
  private Object f = null;

  public Callback(ScriptObjectMirror factoryFn, Object f) {
    this.factoryFn = factoryFn;
    this.f = f;
  }

  @Override
  public Object call(Object param) {
    return this.factoryFn.callMember("call", null, this, this.f, param);
  }
}
