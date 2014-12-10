#= require workflow/steps/step
#= require workflow/steps/step_with_children
#= require workflow/steps/skip_step
#= require workflow/steps/default_option

onWorkflow ->
  class window.Impersonate extends StepWithChildren
    @type = 'impersonate'

    constructor: (attrs) ->
      super(attrs)

      @valid_values = ko.observable attrs.valid_values
      @finish_on_key = ko.observable(attrs.finish_on_key ? capture_default_finish_key)
      @min_input_length = ko.observable(attrs.min_input_length ? capture_default_minimum_input_lenght)
      @max_input_length = ko.observable(attrs.max_input_length ? capture_default_maximum_input_lenght)
      @timeout = ko.observable(attrs.timeout ? capture_default_time_out_in_seconds)
      @number_of_attempts = ko.observable(attrs.number_of_attempts ? capture_default_number_of_attempts)

      @default_skip_step = null
      @default = ko.observable( new DefaultOption(attrs.default, @))
      @current_editing_resource = ko.observable null

      @resources =
        invalid:      new ResourceEditor(@, attrs.invalid_resource)
        instructions: new ResourceEditor(@, attrs.instructions_resource)

      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @is_instructions_resource_invalid = ko.computed () =>
        not @resources.instructions.is_valid()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_instructions_resource_invalid()

      @variable = ko.observable attrs.variable


    get_default_skip_step: () =>
      @default_skip_step ?= new DefaultOption(null, @)

    child_steps: () =>
      if @default().type() == Skip.type
        new Array()
      else
        [@get_default_skip_step(), @default()]

    remove_child_step: (child_step) =>
      super(child_step)
      @default_command_selected('skip')

    button_class: () =>
      'luser'

    @add_to_steps: () ->
      workflow.add_step(new Impersonate)

    @initialize: (hash) ->
      impersonate = new Impersonate(hash)
      return impersonate

    to_hash: () =>
      $.extend(super,
        invalid_resource: @resources.invalid.to_hash()
        instructions_resource: @resources.instructions.to_hash()
        min_input_length: @min_input_length()
        max_input_length: @max_input_length()
        finish_on_key: @finish_on_key()
        timeout: @timeout()
        number_of_attempts: @number_of_attempts()
        default: @default().next_id
        variable: @variable()
      )

    resource: (res) =>
      @resources[res]

    show_resource: (res) =>
      resource = @resources[res]
      @current_editing_resource(resource)

    show_invalid_resource: () =>
      @show_resource('invalid')

    show_instructions_resource: () =>
      @show_resource('instructions')

    available_variables: () =>
      workflow.all_variables().sort()

    default_name: () =>
      'Impersonate'

    can_insert_after: () =>
      super and (@default_command_selected() == Skip.type)
