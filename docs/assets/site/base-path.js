(function () {
  var segs = window.location.pathname.split("/").filter(Boolean);
  var isGithubPagesHost = /\.github\.io$/i.test(window.location.hostname);
  var base = isGithubPagesHost && segs.length > 0 ? "/" + segs[0] : "";
  window.__FG_BASE__ = base;

  var navLinks = document.querySelectorAll("[data-nav-path]");
  navLinks.forEach(function (node) {
    var p = node.getAttribute("data-nav-path") || "/";
    node.setAttribute("href", base + p);
    if (window.location.pathname === base + p || (p !== "/" && window.location.pathname.startsWith(base + p))) {
      node.classList.add("active");
    }
  });
})();
