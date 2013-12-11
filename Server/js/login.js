function login() {
  "use strict";
  var authorization, cfg, password, username;
  username = document.getElementById("login-post-auth-username").value;
  password = document.getElementById("login-post-password").value;
  authorization = window.btoa(username + ":" + password);
  YUI.use('io-base', 'json-parse', function (Y) {
    cfg = {
      headers: {
        Authorization: "Basic " + authorization
      },
      method: "post",
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

