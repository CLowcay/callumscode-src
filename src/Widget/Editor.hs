{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Widget.Editor where

import Import

editorWidget :: Maybe (Route App) -> Bool -> WidgetT App IO ()
editorWidget mDoneRoute isNewPage = do
  addScriptRemote "//cdn.jsdelivr.net/medium-editor/latest/js/medium-editor.min.js"
  addStylesheetRemote "//cdn.jsdelivr.net/medium-editor/latest/css/medium-editor.min.css"
  addStylesheet$ StaticR css_medium_theme_css
  addScript$ StaticR js_jquery_3_2_1_min_js
  addScript$ StaticR js_editor_js

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

    onReady.push(function() {
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

      titleEditor.subscribe('editableInput', function(data, editable) {
        save.innerHTML = "Save";
        save.disabled = false;
      });
      mainEditor.subscribe('editableInput', function(data, editable) {
        save.innerHTML = "Save";
        save.disabled = false;
      });

      ^{saveButtonWidget}
    });
  |]

  where
    saveButtonWidget =
      if isNewPage then [julius|
        save.addEventListener("click",
          function(event) {
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
        save.addEventListener("click",
          function(event) {
            removeLanguageDropdowns();
            $.post(
              window.location.href, {
                title: titleEditor.getContent(),
                content: mainEditor.getContent()
              }, function (data, stat, req) {
                save.innerHTML = "Saved";
                save.disabled = true;
              });
            addLanguageDropdown(languages);
          });
      |]

