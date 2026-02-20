// Thin FFI for SVG DOM creation.
// Only these four functions touch the DOM — everything else is pure.

export const createSvgElement = (tag) => () =>
  document.createElementNS('http://www.w3.org/2000/svg', tag);

export const setAttr = (el) => (k) => (v) => () =>
  el.setAttribute(k, v);

export const appendChild = (parent) => (child) => () =>
  parent.appendChild(child);

export const setTextContent = (el) => (text) => () => {
  el.textContent = text;
};

export const _appendInto = (selector) => (child) => () => {
  const target = document.querySelector(selector);
  if (target) {
    target.innerHTML = '';
    target.appendChild(child);
  }
};
