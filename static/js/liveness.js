for (const liveness of document.querySelectorAll('.liveness')) {
  liveness.addEventListener("click", function (event) {
    $.post(
      liveness.getAttribute('data-url'),
      liveness.checked? {'live':"on"} : {},
      function (data, stat, req) {
        liveness.checked = data.live;
      }
    );
  });
}
