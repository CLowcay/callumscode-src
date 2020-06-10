{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Widget.Editor
  ( editorWidget
  )
where

import           Import

editorWidget :: Maybe (Route App) -> Bool -> WidgetFor App ()
editorWidget mDoneRoute isNewPage = do
  addScriptRemote
    "//cdn.jsdelivr.net/medium-editor/latest/js/medium-editor.min.js"
  addStylesheetRemote
    "//cdn.jsdelivr.net/medium-editor/latest/css/medium-editor.min.css"
  addStylesheet $ StaticR css_medium_theme_css
  addScript $ StaticR js_editor_js

  toWidgetBody [hamlet|
    <div .admin-buttons>
      <button #save .button-control disabled=true>
        Saved
      $maybe doneRoute <- mDoneRoute
        <a .button-control href=@{doneRoute}>
          Done
  |]

  toWidgetBody [julius|
    const languages = [
      'bash',
      'c',
      'c-like',
      'css',
      'haskell',
      'java',
      'javascript',
      'markup',
      'nasm',
      'r',
      'ruby',
      'scala'
    ];

    onReady.push(() => {
      addLanguageDropdown(languages);

      const titleEditor = new MediumEditor('#title', {
        disableReturn:true,
        disableDoubleReturn:true,
        disableExtraSpaces:true,
        toolbar:false
      });

      const mainEditor = new MediumEditor('#post', {
        toolbar: {
          buttons:['bold', 'italic', 'anchor', 'h2', 'h3', 'quote', 'code']
        },
        extensions: {
          'code': new CodeButton()
        }
      });

      const save = document.getElementById('save');

      titleEditor.subscribe('editableInput', (data, editable) => {
        save.innerHTML = "Save";
        save.disabled = false;
      });
      mainEditor.subscribe('editableInput', (data, editable) => {
        save.innerHTML = "Save";
        save.disabled = false;
      });

      ^{saveButtonWidget}
    });
  |]

 where
  saveButtonWidget = if isNewPage
    then [julius|
        save.addEventListener('click', () =>{
          const title = document.createElement('input');
          title.name = 'title';
          title.type = 'text';
          title.value = titleEditor.getContent();

          const content = document.createElement('input');
          content.name = 'content';
          content.type = 'text';
          content.value = mainEditor.getContent();

          const form = document.createElement('form');
          form.method = 'post';
          form.appendChild(title);
          form.appendChild(content);
          form.style.visibility = 'hidden'
          
          document.body.appendChild(form);

          form.submit();
        });
      |]
    else [julius|
        save.addEventListener('click', async () => {
          removeLanguageDropdowns();
          const body = new FormData();
          body.append('title', titleEditor.getContent());
          body.append('content', mainEditor.getContent());
          const response = await fetch(window.location.href, { method: 'POST', body: body });
          if (response.ok) {
            save.innerHTML = 'Saved';
            save.disabled = true;
          }
          addLanguageDropdown(languages);
        });
      |]
