function login() {
  "use strict";
  var auth, cfg, password, username;
  username = document.getElementById("login-post-auth-username").value;
  password = document.getElementById("login-post-password").value;
  auth = window.btoa(username + ":" + password);
  postJSON_auth(
    "/login",
    "",
    function (o) {
      var response, html;
      response = $.parseJSON(o.responseText);
      html = display(response, "response");
      parent.display.document.getElementById("data").innerHTML = html;
    },
    auth
  )
}

function logout() {
  "use strict";
  YUI.use('io-base', 'json-parse', function (Y) {
    var cfg;
    cfg = {
      method: "delete",
      on: {
        success: function (o) {
          var response, html;
          try {
            response = Y.JSON.parse(o.responseText);
          } catch (e) {
            alert("JSON parse failed.");
            return;
          }
          html = display(response, "response");
          parent.display.document.getElementById("data").innerHTML = html;
        }
      }
    };
    Y.io("/login", cfg);
  });
}

