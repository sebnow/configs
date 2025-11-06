{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = [pkgs.camunda-modeler];
  xdg.dataFile."applications/camunda-modeler.desktop" = {
    text = ''
      [Desktop Entry]
      Version=1.0
      Type=Application
      Name=camunda-modeler
      Comment=An integrated modeling solution for BPMN, DMN and Forms based on bpmn.io.
      Exec=${pkgs.camunda-modeler}/bin/camunda-modeler
      Icon=${pkgs.camunda-modeler}/support/icon_128.png
      Categories=Development;ComputerScience;
    '';
  };
}
