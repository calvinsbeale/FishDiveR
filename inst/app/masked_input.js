// Date-time and time boxes for the FishDiveR app (see masked_input() in
// R/app_helpers.R).
//
// Typing: only digits are typed. They fill the letters of the box's pattern
// (e.g. "yyyy-mm-dd hh:mm:ss"), and each separator is added when the next
// digit is typed. Typing a separator early finishes the part being typed
// (2024-3/ becomes 2024-03). Pasted or loaded text with other separators,
// such as "2024/3/5 8:30", is tidied. On leaving the box, a date without a
// time gets 00:00:00, and an invalid date or time is marked.
//
// Pickers: a date-time box shows a calendar as soon as it has the cursor. The
// calendar opens on the years, then the months, then the days. Once the date
// is chosen or typed, a clock replaces it: click the hour, then the minute,
// then the second. Time boxes show the clock. The calendar and clock buttons
// beside the box open them too.
(function ($) {
  'use strict';

  // ---- Typing rules -------------------------------------------------------

  // Lengths of the pattern's digit groups, the separator before each, and
  // their names (with months and minutes told apart)
  function parsePattern(pattern) {
    var re = /([a-z]+)|([^a-z]+)/g, m, sep = '', time = false;
    var out = { lengths: [], seps: [], names: [] };
    while ((m = re.exec(pattern))) {
      if (m[1]) {
        if (m[1] === 'hh') time = true;
        out.lengths.push(m[1].length);
        out.seps.push(sep);
        out.names.push(m[1] === 'mm' ? (time ? 'minute' : 'month') : m[1]);
        sep = '';
      } else {
        sep = m[2];
      }
    }
    return out;
  }

  // Fill the pattern with digits, adding each separator before the next digit
  function fromDigits(digits, pattern) {
    var p = parsePattern(pattern), out = '', d = 0;
    for (var g = 0; g < p.lengths.length && d < digits.length; g++) {
      out += p.seps[g] + digits.substr(d, p.lengths[g]);
      d += p.lengths[g];
    }
    return out;
  }

  function inPatternForm(value, pattern) {
    return fromDigits(value.replace(/\D/g, ''), pattern) === value;
  }

  // Text with its own separators, e.g. "2024/3/5 8:30" or "2024-03-05T08:30Z".
  // Returns null if its parts don't fit the pattern.
  function fromParts(text, pattern) {
    var p = parsePattern(pattern), parts = text.match(/\d+/g);
    if (!parts || parts.length < 2 || parts.length > p.lengths.length) return null;
    var out = '';
    for (var i = 0; i < parts.length; i++) {
      var len = p.lengths[i];
      if (parts[i].length > len || (len > 2 && parts[i].length < len)) return null;
      out += p.seps[i] + ('00' + parts[i]).slice(-len);
    }
    return out;
  }

  function tidy(text, pattern) {
    text = text.trim();
    if (text === '' || inPatternForm(text, pattern)) return text;
    return fromParts(text, pattern) || fromDigits(text.replace(/\D/g, ''), pattern);
  }

  // Complete a value that stops at the end of a part, once it is at least
  // minLength long: "2024-03-05" becomes "2024-03-05 00:00:00"
  function complete(value, pattern, minLength) {
    if (value.length < minLength || value.length >= pattern.length) return value;
    if (/[a-z]/.test(pattern.charAt(value.length))) return value;
    return value + pattern.slice(value.length).replace(/[a-z]/g, '0');
  }

  function isValid(value, pattern) {
    if (value.length !== pattern.length || !inPatternForm(value, pattern)) return false;
    var p = parsePattern(pattern), nums = value.match(/\d+/g), v = {};
    for (var i = 0; i < p.names.length; i++) v[p.names[i]] = parseInt(nums[i], 10);
    if ('month' in v && (v.month < 1 || v.month > 12)) return false;
    if ('dd' in v) {
      var days = new Date(Date.UTC(v.yyyy, v.month, 0)).getUTCDate();
      if (v.dd < 1 || v.dd > days) return false;
    }
    if ('hh' in v && v.hh > 23) return false;
    if ('minute' in v && v.minute > 59) return false;
    if ('ss' in v && v.ss > 59) return false;
    return true;
  }

  function markInvalid(el, bad) {
    $(el).toggleClass('is-invalid', bad);
    $(el).closest('.fd-masked-group').find('.invalid-feedback').toggleClass('d-block', bad);
  }

  // Tidy, complete and mark the box. Tells Shiny if the value changed.
  function check(el) {
    var $el = $(el), pattern = $el.attr('data-pattern');
    var minLength = parseInt($el.attr('data-min-length'), 10);
    var value = tidy(el.value, pattern);
    if (inPatternForm(value, pattern)) value = complete(value, pattern, minLength);
    if (value !== el.value) {
      el.value = value;
      $el.trigger('change');
    }
    markInvalid(el, value !== '' && !isValid(value, pattern));
  }

  // Set the value from a picker, and tell Shiny
  function setValue(el, value) {
    el.value = value;
    markInvalid(el, false);
    $(el).trigger('change');
  }

  function isDateTime(el) {
    return $(el).attr('data-pattern') === 'yyyy-mm-dd hh:mm:ss';
  }

  // The box's date, if a whole valid date has been entered
  function dateOf(el) {
    var date = el.value.slice(0, 10);
    return isValid(date, 'yyyy-mm-dd') ? date : null;
  }

  function timeTextOf(el) {
    return isDateTime(el) ? el.value.slice(11) : el.value;
  }

  // ---- Which picker is open -----------------------------------------------

  var open = { el: null, kind: null, stage: 'hh' };

  function hidePickers() {
    hideCalendar();
    hideClock();
  }

  // Show the picker for the part of the box the cursor is in
  function showPickerFor(el) {
    if (isDateTime(el) && !(dateOf(el) && el.selectionStart > 10)) {
      openCalendar(el);
    } else {
      openClock(el);
    }
  }

  // ---- Calendar -----------------------------------------------------------

  // Shiny loads bootstrap-datepicker as $.fn.bsDatepicker. Each date-time box
  // has its calendar on a hidden, zero-size input just below it (see
  // masked_input()), so the calendar drops down under the box and leaves the
  // box's text alone.
  function anchorOf(el) {
    return $(el).closest('.fd-masked-group').find('.fd-calendar-anchor');
  }

  function openCalendar(el) {
    var $anchor = anchorOf(el);
    if (!$anchor.length || !$.fn.bsDatepicker) return;
    hideClock();

    if (!$anchor.data('datepicker')) {
      $anchor.bsDatepicker({
        format: 'yyyy-mm-dd',
        startView: 2, // years, then months, then days
        maxViewMode: 2,
        autoclose: true,
        todayHighlight: true,
        weekStart: 1,
        orientation: 'bottom left',
        showOnFocus: false,
        forceParse: false
      }).on('changeDate', function () {
        if ($anchor.data('fd-updating')) return;
        var date = $anchor.bsDatepicker('getFormattedDate');
        if (!date) return;
        var time = isValid(el.value, 'yyyy-mm-dd hh:mm:ss') ? el.value.slice(10) : ' 00:00:00';
        setValue(el, date + time);
        // Now the time: select it for typing, and show the clock
        el.focus();
        el.setSelectionRange(11, 19);
        openClock(el, 'hh');
      }).on('hide', function () {
        if (open.el === el && open.kind === 'calendar') open.kind = null;
      });
      // Clicks in the calendar keep the cursor in the box
      $anchor.data('datepicker').picker.addClass('fd-picker');
    }

    // Open on the date already entered, if any
    $anchor.data('fd-updating', true);
    $anchor.bsDatepicker('update', dateOf(el) || '');
    $anchor.data('fd-updating', false);
    $anchor.bsDatepicker('show');
    open.el = el;
    open.kind = 'calendar';
  }

  function hideCalendar() {
    $('.fd-calendar-anchor').each(function () {
      if ($(this).data('datepicker')) $(this).bsDatepicker('hide');
    });
  }

  // ---- Clock --------------------------------------------------------------

  var STAGES = ['hh', 'mm', 'ss'];
  var STAGE_NAMES = { hh: 'hour', mm: 'minute', ss: 'second' };
  var $clock = null;

  function clockElement() {
    if ($clock) return $clock;
    $clock = $('<div class="fd-clock dropdown-menu shadow p-2" role="dialog" aria-label="Choose the time"></div>')
      .appendTo('body')
      // Clicks in the clock keep the cursor in the box
      .on('mousedown', function (e) { e.preventDefault(); })
      .on('click', '[data-stage]', function () {
        open.stage = $(this).attr('data-stage');
        renderClock();
      })
      .on('click', '[data-value]', function () {
        pickTime(parseInt($(this).attr('data-value'), 10));
      });
    return $clock;
  }

  // Hours, minutes and seconds entered so far ("08", "30", null)
  function timeParts(el) {
    var parts = timeTextOf(el).match(/\d+/g) || [];
    return STAGES.map(function (s, i) {
      return parts[i] && parts[i].length === 2 ? parts[i] : null;
    });
  }

  // The part to pick next: the one being typed, or the hour once all are set
  function stageOf(el) {
    var digits = timeTextOf(el).replace(/\D/g, '').length;
    if (digits >= 6 || digits < 2) return 'hh';
    return digits < 4 ? 'mm' : 'ss';
  }

  function renderClock() {
    var el = open.el, parts = timeParts(el), stage = open.stage;
    var current = parts[STAGES.indexOf(stage)];
    var head = STAGES.map(function (s, i) {
      return '<button type="button" class="btn btn-sm ' + (s === stage ? 'btn-primary' : 'btn-outline-secondary') +
        '" data-stage="' + s + '" title="Choose the ' + STAGE_NAMES[s] + '" style="min-width: 2.6rem;">' +
        (parts[i] || '--') + '</button>';
    }).join('<span class="mx-1 fw-bold">:</span>');
    var cells = '';
    for (var n = 0; n < (stage === 'hh' ? 24 : 60); n++) {
      var text = ('0' + n).slice(-2);
      cells += '<button type="button" class="btn btn-sm ' + (text === current ? 'btn-primary' : 'btn-light') +
        '" data-value="' + n + '" style="padding: 2px 0;">' + text + '</button>';
    }
    $clock.html(
      '<div class="d-flex align-items-center justify-content-center mb-2">' + head + '</div>' +
      '<div class="text-center small text-muted mb-1">Choose the ' + STAGE_NAMES[stage] + '</div>' +
      '<div style="display: grid; gap: 2px; grid-template-columns: repeat(' + (stage === 'hh' ? 6 : 10) + ', 2.3rem);">' +
      cells + '</div>'
    );
  }

  // Drop the clock down under the box, inside the window
  function placeClock() {
    var $ref = $(open.el).closest('.input-group');
    if (!$ref.length) $ref = $(open.el);
    var offset = $ref.offset();
    $clock.css({ position: 'absolute', display: 'block', zIndex: 1070, top: offset.top + $ref.outerHeight() + 2, left: offset.left });
    var maxLeft = $(window).scrollLeft() + $(window).width() - $clock.outerWidth() - 8;
    if (offset.left > maxLeft) $clock.css('left', Math.max(0, maxLeft));
  }

  function openClock(el, stage) {
    // A date-time needs its date first
    if (isDateTime(el) && !dateOf(el)) {
      openCalendar(el);
      return;
    }
    hideCalendar();
    clockElement();
    open.el = el;
    open.kind = 'clock';
    open.stage = stage || stageOf(el);
    renderClock();
    placeClock();
  }

  function hideClock() {
    if ($clock) $clock.hide();
    if (open.kind === 'clock') open.kind = null;
  }

  // Set the hour, minute or second, then move on to the next
  function pickTime(n) {
    var el = open.el, i = STAGES.indexOf(open.stage);
    var parts = timeParts(el).map(function (p) { return p || '00'; });
    parts[i] = ('0' + n).slice(-2);
    var time = parts.join(':');
    setValue(el, isDateTime(el) ? dateOf(el) + ' ' + time : time);
    if (i < STAGES.length - 1) {
      open.stage = STAGES[i + 1];
      renderClock();
    } else {
      hideClock();
      el.setSelectionRange(el.value.length, el.value.length);
    }
  }

  // Keep the open picker in step with typing: once the date is typed, show the
  // clock; while the time is typed, show the part being typed; once it is all
  // typed, close the clock
  function followTyping(el) {
    if (open.el !== el || !open.kind) return;
    if (isDateTime(el) && !dateOf(el)) {
      if (open.kind === 'clock') openCalendar(el);
      return;
    }
    var digits = timeTextOf(el).replace(/\D/g, '').length;
    if (digits >= 6) {
      hidePickers();
    } else if (open.kind === 'calendar') {
      openClock(el, stageOf(el));
    } else {
      open.stage = stageOf(el);
      renderClock();
    }
  }

  // ---- Events -------------------------------------------------------------

  $(document).on('input', '.fd-masked', function (e) {
    var el = this, pattern = $(el).attr('data-pattern');
    var raw = el.value, caret = el.selectionStart;
    // Pasted, dropped or autofilled text, rather than one typed key
    var ev = e.originalEvent || {};
    var pasted = /^insertFrom|^insertReplacement/.test(ev.inputType || '') ||
      (ev.inputType === 'insertText' && ev.data && ev.data.length > 1);
    var value = pasted ? tidy(raw, pattern) : fromDigits(raw.replace(/\D/g, ''), pattern);
    markInvalid(el, false);
    if (value !== raw) {
      el.value = value;
      // Shiny has already read the typed text, so send it the new value
      $(el).trigger('change');
      // Keep the caret after the same number of digits
      var pos = value.length;
      if (!pasted) {
        var digits = raw.slice(0, caret).replace(/\D/g, '').length, count = 0;
        pos = 0;
        while (pos < value.length && count < digits) {
          if (/\d/.test(value.charAt(pos))) count++;
          pos++;
        }
      }
      el.setSelectionRange(pos, pos);
    }
    followTyping(el);
  });

  // A typed separator (or any other character) is not added, but finishes the
  // part being typed: 2024-3 becomes 2024-03
  $(document).on('beforeinput', '.fd-masked', function (e) {
    var ev = e.originalEvent;
    if (!ev || ev.inputType !== 'insertText' || !ev.data || !/^\D+$/.test(ev.data)) return;
    e.preventDefault();
    var el = this, value = el.value;
    if (el.selectionStart !== value.length || el.selectionEnd !== value.length) return;
    var p = parsePattern($(el).attr('data-pattern')), parts = value.match(/\d+/g);
    if (!parts) return;
    var i = parts.length - 1, last = parts[i], len = p.lengths[i];
    if (len <= 2 && last.length < len) {
      el.value = value.slice(0, value.length - last.length) + ('00' + last).slice(-len);
      $(el).trigger('change');
      followTyping(el);
    }
  });

  // Show a picker as soon as the box has the cursor, by clicking or tabbing.
  // Wait for the click to place the cursor.
  $(document).on('focus', '.fd-masked', function () {
    var el = this;
    setTimeout(function () {
      if (document.activeElement === el) showPickerFor(el);
    }, 0);
  });
  $(document).on('click', '.fd-masked', function () {
    showPickerFor(this);
  });

  $(document).on('keydown', '.fd-masked', function (e) {
    if (e.key === 'Escape') hidePickers();
  });

  // Check on leaving the box, and when the server sets the value (not while
  // typing, when the value is often incomplete)
  $(document).on('blur', '.fd-masked', function () {
    hidePickers();
    check(this);
  });
  $(document).on('change', '.fd-masked', function () {
    if (document.activeElement !== this) check(this);
  });

  $(window).on('resize', hidePickers);

  // Clicks in the calendar, and on the buttons, keep the cursor in the box
  $(document).on('mousedown', '.fd-picker, .fd-calendar, .fd-clock-button', function (e) {
    e.preventDefault();
  });

  // The buttons beside the box open its calendar or clock, or close it
  $(document).on('click', '.fd-calendar, .fd-clock-button', function () {
    var el = document.getElementById($(this).attr('data-fd-input'));
    if (!el) return;
    var kind = $(this).hasClass('fd-calendar') ? 'calendar' : 'clock';
    // The calendar closes itself when the button is pressed, so check the clock
    if (kind === 'clock' && open.el === el && open.kind === 'clock') {
      hideClock();
      return;
    }
    el.focus();
    // After the focus handler, so this picker wins
    setTimeout(function () {
      if (kind === 'calendar') openCalendar(el); else openClock(el, 'hh');
    }, 0);
  });
})(jQuery);
