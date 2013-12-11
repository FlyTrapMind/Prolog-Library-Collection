function postUser() {
  // Use stricts mode
  // (ECMAScript 5 exclusion for ECMASCript 3 deprecated features).
  "use strict";
  // Make function-scoped hoisting explicit.
  var user, password, content;
  user = document.getElementById("user-post-user").value;
  password = document.getElementById("user-post-password").value;
  content = document.getElementById("user-post-content").value;
  YUI().use('io-base', function (Y) {
    var cfg = {
      data: content,
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
          var html = display(response, "response");
          parent.display.document.getElementById("data").innerHTML = html;
        }
      }
    };
    Y.io("/users?user=" + user + "&password=" + password, cfg);
  });
}

function deleteUser() {
  var user = document.getElementById("user-delete-user").value;
  YAHOO.util.Connect.asyncRequest("DELETE", "/users?user=" + user, {
    success: function(o) {
      var response = YAHOO.lang.JSON.parse(o.responseText);
      var html = display(response, "response");
      parent.display.document.getElementById("data").innerHTML = html;
    }
  }, "");
}

function getUsers() {
  var user = document.getElementById("user-get-user").value;
  YAHOO.util.Connect.asyncRequest("GET", "/users?user=" + user, {
    success: function(o) {
      var response = YAHOO.lang.JSON.parse(o.responseText);
      var html = display(response, "response");
      parent.display.document.getElementById("data").innerHTML = html;
    }
  });
}

function postSetting() {
  var module = document.getElementById("setting-post-module").value;
  var setting = document.getElementById("setting-post-setting").value;
  var content = document.getElementById("setting-post-content").value;
  YAHOO.util.Connect.asyncRequest("POST", "/settings?module=" + module + "&setting=" + setting, {
    success: function(o) {
      var response = YAHOO.lang.JSON.parse(o.responseText);
      var html = display(response, "response");
      parent.display.document.getElementById("data").innerHTML = html;
    }
  }, content);
}

function getSettings() {
  var module = document.getElementById("setting-get-module").value;
  var setting = document.getElementById("setting-get-setting").value;
  YAHOO.util.Connect.asyncRequest("GET", "/settings?module=" + module + "&setting=" + setting, {
    success: function(o) {
      var response = YAHOO.lang.JSON.parse(o.responseText);
      var html = display(response, "response");
      parent.display.document.getElementById("data").innerHTML = html;
    }
  });
}

function getStatistics() {
  YAHOO.util.Connect.asyncRequest("GET", "/statistics", {
    success: function(o) {
      var response = YAHOO.lang.JSON.parse(o.responseText);
      var html = display(response, "response");
      parent.display.document.getElementById("data").innerHTML = html;
    }
  });
}

