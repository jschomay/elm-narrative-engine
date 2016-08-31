require( './styles/reset.css' );
require( './styles/main.css' );
require( './styles/dark-theme.css' );

// inject bundled Elm app
var Elm = require( './Main' );
Elm.Main.fullscreen();
