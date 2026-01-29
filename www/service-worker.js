self.addEventListener('fetch', function(event) {
  event.respondWith(
    fetch(event.request).catch(function() {
      return new Response('Estás fuera de línea, pero la App está instalada.');
    })
  );
});