function login() {
  "use strict";
  var auth, cfg, password, username;
  username = $("#login-post-auth-username").val();
  password = $("#login-post-password").val();
  auth = window.btoa(username + ":" + password);
  postJSON_auth("/login", "", successJSON, auth);
}

function logout() {
  "use strict";
  deleteJSON("/login", "", successJSON);
}

