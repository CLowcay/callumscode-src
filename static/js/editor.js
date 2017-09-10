/**
 * Code block button extension for MediumEditor
 * */
const CodeButton = MediumEditor.Extension.extend({
  name: 'code',

  init: function () {
    const editor = this;

    editor.button = editor.document.createElement('button');
    editor.button.classList.add('medium-editor-action');
    editor.button.innerHTML = '<b>CODE</b>';

    editor.on(editor.button, 'click', editor.handleClick.bind(editor));
    editor.subscribe('editableInput', function(data, editable) {
      if (cleanupBlocks('code')) editor.base.checkContentChanged();
      addLanguageDropdown(languages);
    });
  },

  getButton: function () {
    return this.button;
  },

  lastBlock: undefined,

  handleClick: function (event) {
    const sn = document.getSelection().isCollapsed?
      this.lastBlock : document.getSelection().focusNode;
    if (sn == undefined) return;

    const e = sn.nodeType === 1 ? sn : sn.parentElement;
    const ep = e.parentNode;

    if (e.nodeName.toLowerCase() === 'code') {
      if (ep.nodeName.toLowerCase() === 'p') {
        ep.innerHTML = e.innerHTML;
        this.lastBlock = ep;
      } else if (ep.nodeName.toLowerCase() === 'pre') {
        const p = document.createElement('p');
        p.innerHTML = e.innerHTML;
        ep.parentNode.replaceChild(p, ep);
        this.lastBlock = p;
      } else {
        const p = document.createElement('p');
        p.innerHTML = e.innerHTML;
        e.parentNode.replaceChild(p, e);
        this.lastBlock = p;
      }
    } else {
      const c = document.createElement('code');
      c.innerHTML = e.innerHTML;
      
      if (e.nodeName.toLowerCase() === 'pre') {
        e.innerHTML = '';
        e.appendChild(c);
      } else {
        const pre = document.createElement('pre');
        pre.appendChild(c);
        e.parentNode.replaceChild(pre, e);
      }

      this.lastBlock = c;
    }

    this.base.checkContentChanged();
  },

  isAlreadyApplied: function (node) {
    return node.nodeName.toLowerCase() === 'code' && node.parentNode &&
      node.parentNode.nodeName.toLowerCase() === 'pre';
  },

  isActive: function () {
    return this.button.classList.contains('medium-editor-button-active');
  },

  setInactive: function () {
    this.button.classList.remove('medium-editor-button-active');
  },

  setActive: function () {
    this.button.classList.add('medium-editor-button-active');
  }
});

/**
 * Make sure the editor stays in a clean state
 * returns: true if any changes were made
 * */
function cleanupBlocks(kind) {
  let changed = false;

  // Cannot start a <p> with a code block
  for (const p of document.getElementsByTagName('p')) {
    if (p.firstChild && p.firstChild.nodeName.toLowerCase() === kind) {
      const n = p.nextElementSibling;
      if (n.nodeName.toLowerCase() === 'pre' &&
        n.firstChild && n.firstChild.nodeName.toLowerCase() === kind
      ) {
        n.firstChild.innerHTML = p.firstChild.innerHTML + n.firstChild.innerHTML;
        p.parentNode.removeChild(p);
      } else if (p.firstChild.textContent.trim() === '') {
        p.parentNode.removeChild(p);
      } else {
        const pre = document.createElement('pre');
        pre.innerHTML = p.innerHTML;
        p.parentNode.replaceChild(pre, p);
      }
      changed = true;
    }
  }

  // Cannot have empty code blocks
  for (const code of document.getElementsByTagName(kind)) {
    if (code.textContent.trim() === "") {
      code.parentNode.removeChild(code);
      changed = true;
    }
  }

  // prevent paragraphs from consolidating into pre blocks
  for (const pre of document.getElementsByTagName('pre')) {
    const reference = pre.nextElementSibling;

    if (pre.hasChildNodes()) {
      const safeChildren = [];

      for (const child of pre.childNodes) safeChildren.push(child);
      
      for (const child of safeChildren) {
        if (child.nodeType === 3) {
          const p = document.createElement('p');
          p.innerHTML = child.data;
          if (reference) pre.parentNode.insertBefore(p, reference);
          else pre.parentNode.appendChild(p);
          pre.removeChild(child);

          // a nasty hack.  This forces the MediumEditor to recognise
          // that the DOM has changed, so editableInput event will be fired
          // even if the user tries a second time to coalesce paragraphs and
          // pre's.
          pre.firstChild.innerHTML = pre.firstChild.innerHTML + " ";
          changed = true;
        }
      }
    }
  }

  return changed;
}

function removeLanguageDropdowns() {
  const ss = [];
  for (const s of document.querySelectorAll(".language-select")) ss.push(s);
  for (const s of ss) s.parentNode.removeChild(s);
}

function addLanguageDropdown(languages) {
  for (const code of document.getElementsByTagName('code')) {
    if (!code.firstChild || code.firstChild.nodeName.toLowerCase() !== 'select') {
      const select = document.createElement('select');
      select.setAttribute('class', 'language-select');
      select.setAttribute('contenteditable', 'false');

      const opnone = document.createElement('option');
      opnone.setAttribute('value', 'none');
      opnone.innerText = 'None';
      select.appendChild(opnone);

      for (const l of languages) {
        const op = document.createElement('option');
        op.setAttribute('value', l);
        op.innerText = l;
        select.appendChild(op);
      }

      if (!code.firstChild) {
        code.appendChild(select);
      } else {
        code.insertBefore(select, code.firstChild);
      }

      const oldLang = code.getAttribute('class');
      if (oldLang && oldLang.startsWith('language')) {
        select.value = oldLang.substring(9);
      }

      let mousein = false;

      code.addEventListener('mouseover', function(event) {
        mousein = true;
        select.style.visibility = 'visible';
        select.style.opacity = 1;
      });

      code.addEventListener('mouseout', function(event) {
        mousein = false;
        if (document.activeElement !== select) {
          select.style.visibility = 'hidden';
          select.style.opacity = 0;
        }
      });

      select.addEventListener('blur', function(event) {
        if (!mousein) {
          select.style.visibility = 'hidden';
          select.style.opacity = 0;
        }
      });

      select.addEventListener('change', function(event) {
        if (select.value === 'none') {
          code.removeAttribute('class');
        } else {
          code.setAttribute('class', 'language-' + select.value);
        }
      });
    }
  }
}

