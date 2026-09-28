// These controls deliberately need no Shiny connection: fatal errors close it.
(function () {
  if (window.kwallmErrorReportControls) return;
  window.kwallmErrorReportControls = true;

  document.addEventListener("click", function (event) {
    const button = event.target.closest("[data-error-report-action]");
    if (!button) return;
    const container = button.closest(".kwallm-error-report");
    if (!container) return;
    const textarea = container.querySelector("textarea");
    const status = container.querySelector(".kwallm-error-report-status");
    const text = textarea.value;

    function fallbackCopy() {
      container.querySelector("details").open = true;
      textarea.focus();
      textarea.select();
      let copied = false;
      try { copied = document.execCommand("copy"); } catch (_) { /* manual copy */ }
      status.textContent = copied
        ? container.dataset.copySuccess : container.dataset.copyFailure;
    }

    if (button.dataset.errorReportAction === "copy") {
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text).then(function () {
          status.textContent = container.dataset.copySuccess;
        }).catch(fallbackCopy);
      } else {
        fallbackCopy();
      }
    } else if (button.dataset.errorReportAction === "download") {
      const url = URL.createObjectURL(new Blob([text], { type: "text/plain;charset=utf-8" }));
      const link = document.createElement("a");
      link.href = url;
      link.download = "kwallm-error-" + container.dataset.errorId + ".txt";
      document.body.appendChild(link);
      link.click();
      link.remove();
      setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
    }
  });
})();
