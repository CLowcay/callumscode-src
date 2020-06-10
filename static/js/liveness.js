for (const liveness of document.querySelectorAll('.liveness')) {
  liveness.addEventListener('click', async () => {
    const body = new FormData();
    if (liveness.checked) body.append('live', 'on');

    const url = liveness.getAttribute('data-url');
    const response = await fetch(url, { method: 'POST', body: body });
    if (response.ok) liveness.checked = (await response.json()).live;
  });
}
