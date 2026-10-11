// Interactive tests for docs/index.html in jsdom.  Run via `make docs-ui` (installs jsdom into .cache/).
const path = require('path');
const { JSDOM, VirtualConsole } = require(process.env.JSDOM_PATH || 'jsdom');
const file = process.argv[2];
const errors = [];
const vc = new VirtualConsole();
vc.on('jsdomError', e => errors.push(String(e.stack || e)));
vc.on('error', e => errors.push('console.error: ' + e));
JSDOM.fromFile(file, {
  runScripts: 'dangerously', pretendToBeVisual: true, virtualConsole: vc,
  beforeParse(w) {
    w.IntersectionObserver = class { observe() {} };
    w.HTMLElement.prototype.scrollIntoView = () => {};
    w.matchMedia = w.matchMedia || (() => ({ matches: false, addEventListener() {} }));
  },
}).then(async dom => {
  const { window } = dom, { document } = window;
  let pass = 0, fail = 0;
  const ok = (c, m) => { if (c) pass++; else { fail++; console.log('FAIL:', m); } };
  const $ = s => document.querySelector(s), $$ = s => Array.from(document.querySelectorAll(s));
  const key = (code, o = {}, target = window) => {
    const e = new window.KeyboardEvent('keydown', { code, key: o.key || code.replace(/^Key|^Digit/, '').toLowerCase(), bubbles: true, cancelable: true, ...o });
    (target.dispatchEvent ? target : window).dispatchEvent(e); return e;
  };
  const tick = () => new Promise(r => setTimeout(r, 5));
  await tick();
  const DATA = JSON.parse(document.getElementById('ide-data').textContent);
  const nKb = DATA.keybinds.length;

  ok(errors.length === 0, 'no script errors: ' + errors.join(' | '));
  const text = document.querySelector('main').textContent;  // visible content, not script source
  ok(!/\[object |undefined|\bNaN\b/.test(text), 'no [object]/undefined/NaN in rendered text');

  // data-driven rendering
  ok($$('.kbrow').length === nKb, `renders ${nKb} keybinding rows, got ` + $$('.kbrow').length);
  ok($$('#stats .stat').length === 5, 'five stats');
  ok($$('#wftabs [role=tab]').length === DATA.workflows.length, 'one tab per workflow');
  ok($$('#fgrid article').length >= 20, 'feature cards render');
  ok($$('#tooltable tr').length === DATA.tools.length, 'one row per tool');
  ok($$('#modtable tr').length === DATA.modules.length, 'one row per module');

  // search + category filters
  const search = $('#kbsearch');
  search.value = 'azdo'; search.dispatchEvent(new window.Event('input'));
  const azdoRows = $$('.kbrow').length;
  ok(azdoRows > 5 && azdoRows < 30, 'search "azdo" narrows rows (' + azdoRows + ')');
  search.value = 'zzzz-nothing'; search.dispatchEvent(new window.Event('input'));
  ok($('.empty') && $$('.kbrow').length === 0, 'empty state for no match');
  search.value = 'alt+1'; search.dispatchEvent(new window.Event('input'));
  ok($$('.kbrow').length === 1, 'IDE badge text is searchable');
  search.value = ''; search.dispatchEvent(new window.Event('input'));
  const chip = $$('#kbchips .chip').find(c => c.textContent === 'AI'); chip.click();
  const nAI = DATA.keybinds.filter(k => k.cat === 'AI').length;
  ok($$('.kbrow').length === nAI, `AI category has ${nAI} rows, got ` + $$('.kbrow').length);
  $$('#kbchips .chip')[0].click();
  ok($$('.kbrow').length === nKb, 'All restores every row');
  // categories come from the data, so new ones (e.g. Help & docs) appear automatically
  const cats = [...new Set(DATA.keybinds.map(k => k.cat))];
  ok($$('#kbchips .chip').length === cats.length + 1, 'one chip per data category plus All');
  const helpChip = $$('#kbchips .chip').find(c => c.textContent === 'Help & docs');
  ok(!!helpChip, 'Help & docs category chip exists');
  if (helpChip) { helpChip.click(); ok($$('.kbrow').length === DATA.keybinds.filter(k => k.cat === 'Help & docs').length, 'Help & docs rows'); $$('#kbchips .chip')[0].click(); }

  // every zone panel renders without stray null/undefined
  $$('.zone[data-zone]').forEach(z => { z.click();  /* includes the non-button editor mock */ ok(!/\bnull\b|undefined|\[object/.test($('#zoneinfo').textContent), 'zone panel clean: ' + z.dataset.zone); });

  // layout toggles
  const treeBtn = $('[data-toggle=tree]');
  ok(!$('.z-tree').hidden, 'tree visible initially');
  treeBtn.click(); ok($('.z-tree').hidden && treeBtn.getAttribute('aria-pressed') === 'false', 'toggle hides tree');
  treeBtn.click(); ok(!$('.z-tree').hidden, 'toggle shows tree');
  $('[data-zone=struct]').click(); ok($('#zoneinfo').textContent.includes('imenu-list'), 'zone info updates');

  // try-keys mode
  $('#trykeys').click();
  ok($('#trykeys').getAttribute('aria-pressed') === 'true', 'try mode on');
  key('KeyC', { ctrlKey: true }); key('KeyW');
  let e = key('Digit1');
  ok(e.defaultPrevented, 'C-c w 1 default prevented in try mode');
  ok($('#tryout').textContent.includes('ide-toggle-project-tree'), 'C-c w 1 resolves to project tree: ' + $('#tryout').textContent);
  ok($('.z-tree').hidden, 'C-c w 1 toggled the tree in the diagram');
  key('KeyC', { ctrlKey: true }); key('KeyW'); key('Digit1'); ok(!$('.z-tree').hidden, 'C-c w 1 again re-shows tree');
  key('Period', { altKey: true, key: '≥' });
  ok($('#tryout').textContent.includes('xref-find-definitions'), 'M-. resolves');
  key('KeyC', { ctrlKey: true });
  ok(/then/.test($('#tryout').textContent) && $('#tryout').textContent.includes('Docker'), 'C-c shows which-key candidates');
  key('KeyD'); ok($('#tryout').textContent.includes('Kubernetes') && $('#tryout').textContent.includes('Azure DevOps center'), 'C-c d lists next keys');
  key('KeyO'); ok($('#tryout').textContent.includes('azdo-dispatch'), 'C-c d o resolves: ' + $('#tryout').textContent);
  key('KeyF', { ctrlKey: true, shiftKey: true, key: 'F' });
  ok($('#tryout').textContent.includes('consult-ripgrep'), 'C-S-f resolves: ' + $('#tryout').textContent);
  key('F10', { shiftKey: true, key: 'F10' });
  ok($('#tryout').textContent.includes('ide-run-current'), 'S-<f10> resolves: ' + $('#tryout').textContent);
  key('F12', { altKey: true, key: 'F12' });
  ok($('#tryout').textContent.includes('ide-toggle-terminal'), 'M-<f12> resolves: ' + $('#tryout').textContent);
  key('KeyC');
  ok($('#tryout').textContent.includes('PR diff'), 'plain c points to contextual binding: ' + $('#tryout').textContent);
  key('KeyZ', { ctrlKey: true }); ok($('#tryout').textContent.includes('not bound'), 'unbound chord explained');
  key('Escape'); key('Escape');
  ok($('#trykeys').getAttribute('aria-pressed') === 'false', 'Esc Esc leaves try mode');
  e = key('Digit1', { altKey: true });
  ok(!e.defaultPrevented, 'keys are not captured when try mode is off');

  // "/" focuses search
  key('Slash', { key: '/' });
  ok(document.activeElement === search, '/ focuses the search box');
  search.blur();

  // menu simulator
  const win = $('#menuwin');
  ok(win.textContent.includes('Services & DevOps'), 'services menu shown first');
  const press = (k, o = {}) => win.dispatchEvent(new window.KeyboardEvent('keydown', { key: k, bubbles: true, cancelable: true, ...o }));
  press('o'); ok(win.querySelector('h3').textContent === 'Azure DevOps', 'o descends to Azure DevOps menu');
  press('p'); ok(win.querySelector('h3').textContent.includes('Pull requests'), 'p descends to PR menu');
  press('v'); ok(win.querySelector('h3').textContent === 'Vote picker', 'v descends to vote picker');
  press('a'); ok(win.querySelector('.menu-out').textContent.includes('vote 10'), 'a explains vote 10');
  press('Backspace'); ok(win.querySelector('h3').textContent.includes('Pull requests'), 'Backspace goes up');
  press('m'); ok(win.querySelector('.menu-out').textContent.includes('azdo-pr-complete'), 'm explains completion');
  $$('#menulist .chip').find(c => c.textContent === 'Terraform & Packer').click();
  press('P'); press('b'); ok(win.querySelector('.menu-out').textContent.includes('packer build'), 'two-key entry "P b" works: ' + win.querySelector('.menu-out').textContent);
  press('a'); ok(win.querySelector('.menu-out').textContent.includes('terraform apply'), 'a runs apply description');

  // workflows
  const tabs = $$('#wftabs [role=tab]');
  tabs[1].click();
  ok($('#wfbody').textContent.includes('push menu'), 'second workflow shown');
  ok($$('#wfbody .steps li').length >= 4, 'steps rendered');
  ok($$('#wfbody kbd').length > 3, 'keycaps rendered inside steps');
  tabs[1].focus(); tabs[1].dispatchEvent(new window.KeyboardEvent('keydown', { key: 'ArrowRight', bubbles: true, cancelable: true }));
  ok($('#wftabs [aria-selected=true]').dataset.id === 'commit', 'ArrowRight moves to next tab');

  // features
  $$('#fchips .chip').find(c => c.textContent === 'AI').click();
  ok($$('#fgrid article').length === DATA.features.filter(f => f.cat === 'AI').length, 'AI features filtered: ' + $$('#fgrid article').length);

  // tools
  $$('#tchips .chip').find(c => c.textContent === 'Cloud').click();
  ok($$('#tooltable tr').length === DATA.tools.filter(t => t.cat === 'Cloud').length, 'cloud tools: ' + $$('#tooltable tr').length);

  // languages cards, filter, expand, and deep links
  const nLangs = (DATA.languages || []).length;
  ok(nLangs >= 8, `at least 8 languages in data, got ${nLangs}`);
  ok($$('.lcard').length === nLangs, `renders ${nLangs} language cards`);

  // language filter
  const lsearch = $('#langsearch');
  lsearch.value = 'rust'; lsearch.dispatchEvent(new window.Event('input'));
  const visibleRust = $$('.lcard:not([hidden])').length;
  ok(visibleRust >= 1 && visibleRust < nLangs, `filter "rust" narrows cards (${visibleRust})`);
  lsearch.value = 'zzzz-nothing'; lsearch.dispatchEvent(new window.Event('input'));
  ok($('#lgrid .empty') && $$('.lcard:not([hidden])').length === 0, 'empty state for no language match');
  lsearch.value = ''; lsearch.dispatchEvent(new window.Event('input'));
  ok($$('.lcard:not([hidden])').length === nLangs, 'clearing filter restores all language cards');

  // expand / collapse all
  const toggleBtn = $('#langtoggle');
  ok(toggleBtn.textContent.includes('Expand all'), 'initially Expand all');
  toggleBtn.click();
  ok($$('.lcard details').every(d => d.open), 'toggle opens all language details');
  ok(toggleBtn.textContent.includes('Collapse all'), 'toggle button says Collapse all');
  toggleBtn.click();
  ok($$('.lcard details').every(d => !d.open), 'toggle closes all language details');
  ok(toggleBtn.textContent.includes('Expand all'), 'toggle button says Expand all again');

  // hashchange deep link to language card
  window.location.hash = '#lang-rust';
  window.dispatchEvent(new window.HashChangeEvent('hashchange'));
  const rustCard = $('#lang-rust');
  ok(rustCard && rustCard.querySelector('details').open, '#lang-rust deep link opens its details');

  // menu chips come from the data
  const linked = id => DATA.menus.some(m => m.groups.some(g => g.items.some(i => i.menu === id)));
  const tops = DATA.menus.filter(m => m.top !== undefined ? m.top : !linked(m.id));
  ok($$('#menulist .chip').length === tops.length, 'menu chips match top-level menus');
  ok(tops.some(m => m.id === 'votes'), 'vote picker remains a top-level chip');

  // theme
  const th = $('#theme'); th.click(); ok(document.documentElement.dataset.theme === 'dark', 'theme -> dark');
  th.click(); ok(document.documentElement.dataset.theme === 'light', 'theme -> light');
  th.click(); ok(document.documentElement.dataset.theme === 'auto', 'theme -> auto');

  // a11y structure
  ok($$('main section[id]').length === $$('#navlist a').length, 'one section per nav link');
  ok($$('#extabs [role=tab]').length === DATA.examples.length, 'one tab per shipped example');
  ok($$('#recipes article').length === DATA.recipes.length, 'one card per recipe');
  $$('#extabs [role=tab]')[1].click();
  ok(document.querySelector('#exbody pre').textContent.length > 50 && /user\//.test(document.querySelector('#exbody .fname').textContent), 'example shows its files and code');
  ok($$('section[aria-labelledby]').every(s => document.getElementById(s.getAttribute('aria-labelledby'))), 'aria-labelledby targets exist');
  ok($$('button').every(b => b.textContent.trim() || b.getAttribute('aria-label')), 'every button has an accessible name');
  ok($$('h1').length === 1, 'single h1');
  ok($('html').getAttribute('lang') === 'en', 'html lang');
  ok(errors.length === 0, 'still no script errors: ' + errors.join(' | '));

  // ?q= deep link pre-fills the search
  const dom2 = await JSDOM.fromFile(file, { url: 'file://' + path.resolve(file) + '?q=azdo-pr-vote#keys', runScripts: 'dangerously', pretendToBeVisual: true, virtualConsole: vc,
    beforeParse(w) { w.IntersectionObserver = class { observe() {} }; w.HTMLElement.prototype.scrollIntoView = () => {}; } });
  await tick();
  const d2 = dom2.window.document;
  ok(d2.querySelector('#kbsearch').value === 'azdo-pr-vote', 'q param fills the search box');
  const rows2 = d2.querySelectorAll('.kbrow').length;
  ok(rows2 >= 1 && rows2 < nKb, 'q param filters the rows (' + rows2 + ')');

  // ?q= deep link to #languages pre-fills the language filter
  const domLang = await JSDOM.fromFile(file, { url: 'file://' + path.resolve(file) + '?q=rust#languages', runScripts: 'dangerously', pretendToBeVisual: true, virtualConsole: vc,
    beforeParse(w) { w.IntersectionObserver = class { observe() {} }; w.HTMLElement.prototype.scrollIntoView = () => {}; } });
  await tick();
  const dLang = domLang.window.document;
  ok(dLang.querySelector('#langsearch').value === 'rust', 'q param fills the language search box');
  const langRows = dLang.querySelectorAll('.lcard:not([hidden])').length;
  ok(langRows >= 1 && langRows < nLangs, `q param filters language cards (${langRows})`);

  console.log(`jsdom UI test: ${pass} passed, ${fail} failed`);
  process.exit(fail ? 1 : 0);
}).catch(e => { console.error('HARNESS ERROR', e); process.exit(2); });
