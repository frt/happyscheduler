import { TestBed, async, inject } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpModule } from '@angular/http'

import { TaskListComponent } from './task-list.component';

describe('TaskListComponent', () => {

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [
                TaskListComponent
            ],
            imports: [
                RouterTestingModule,
                HttpModule
            ],
            providers : [
                HttpModule,
            ]
        }).compileComponents();
    }));

    it('should not have an <hr> element',  async(() => {
        const fixture = TestBed.createComponent(TaskListComponent);
        const nodes = fixture.debugElement.nativeElement;
        console.log(nodes.querySelectorAll('hr'));
        expect(nodes.querySelectorAll('hr').length).toEqual(0);
    }));
});
