// pull in desired CSS/SASS files
require( './styles/main.scss' );
var $ = jQuery = require( '../../node_modules/jquery/dist/jquery.js' );           // <--- remove if jQuery not needed
require( '../../node_modules/semantic-ui-css/semantic.min.js' );   // <--- remove if Semantic UI is not needed

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
const app = Elm.Main.embed( document.getElementById( 'main' ) );

console.log(app);

app.ports.outgoing.subscribe(function(msg_with_id) {
    msg = Object.assign({}, msg_with_id.msg);
    msg.id = msg_with_id.id;
    console.log(msg_with_id);
    web3.currentProvider.sendAsync(msg, function(err, response) {
        if (err) {
            console.log("Web3 Response error", err);
        } else {
            console.log("WEB3 Response:", response);
            app.ports.incoming.send({id: msg_with_id.id, msg: response});
        }
    });
    // const id = msg_with_id.id;
    // const request = msg_with_id.msg;
    // console.log("RECEIVED MESSAGE: ", msg_with_id);
    // response = "The quick brown fox!";

    // app.ports.incoming.send({
    //     id: id,
    //     msg: response
    // });
});
