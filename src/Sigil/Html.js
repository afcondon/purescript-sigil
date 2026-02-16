// Thin FFI for HTML injection into DOM.
// Single function: set innerHTML of an element found by CSS selector.

export const _renderInto = (selector) => (html) => () => {
  const el = document.querySelector(selector);
  if (el) el.innerHTML = html;
};
